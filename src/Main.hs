{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding (log)
import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Writer hiding ((<>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Digest.Pure.SHA
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable
import Network.HTTP.Conduit hiding (path)
import Network.HTTP.Types.Status
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process
import Text.XML.Light

data HavenEnv = HavenEnv
  { _havenEnv_manager :: Manager
  , _havenEnv_repos :: Map String String
  , _havenEnv_m2Local :: FilePath
  }


log :: MonadIO m => T.Text -> m ()
log t = liftIO $ T.hPutStrLn stderr t *> hFlush stderr

-- | Takes multiple maven package descriptions as command line arguments
-- and finds the dependencies of those maven packages.
-- Package descriptions should be of the form @groupid:artifactid:version@
main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  [pomXml] <- getArgs
  Just repos <- fmap parseRepos . parseXMLDoc <$> BS.readFile pomXml

  let
    withStorePath :: (MonadIO m, MonadMask m) => (String -> m a) -> m a
    withStorePath f = do
      storePath' <- liftIO $ lookupEnv "HAVEN_STORE_PATH"
      case storePath' of
        Just storePath | not (null storePath) -> f storePath
        _ -> withSystemTempDirectory "m2" f

  (_, mavenNixs) <- runWriterT $
    withStorePath $ \m2Repo -> withSystemTempFile "out.txt" $ \tmpFile hTmpFile -> do
      for_ ["tree", "resolve"] $ \mode -> do
        let havenEnv = HavenEnv mgr repos m2Repo
        hProc <- liftIO $ runProcess
          "mvn"
          ["-f", pomXml, "dependency:" <> mode, "-Dverbose", "-DoutputFile=" <> tmpFile, "-Dmaven.repo.local=" <> m2Repo]
          Nothing
          Nothing
          Nothing
          (Just stderr)
          Nothing
        ExitSuccess <- liftIO $ waitForProcess hProc

        _ <- liftIO $ T.hGetLine hTmpFile -- skip first line which is the root of the tree or a blank line
        fix $ \loop -> do
          e <- liftIO $ hIsEOF hTmpFile
          unless e $ do
            line <- fmap T.strip $ liftIO $ T.hGetLine hTmpFile
            log $ ">> " <> line

            unless (T.null line || line == "The following files have been resolved:") $ do
              let (packageDesc, notes) = T.span (/= ' ') $ T.dropWhile (not . isAlphaNum) line
              case False of -- "omitted" `T.isInfixOf` notes
                True -> loop <* log ("Skipping omitted: " <> packageDesc <> " (" <> notes)
                False -> case map T.unpack $ T.splitOn ":" $ T.strip packageDesc of
                  [groupId, artifactId, fileType, version, _] -> do
                    let maven = Maven groupId artifactId version
                    mMvnNix <- runReaderT (runMaybeT $ fetch fileType maven) havenEnv
                    case mMvnNix of
                      Just mvnNix -> tell $ Set.fromList mvnNix
                      Nothing -> error $ "Failed for " <> unlines [show fileType, show maven]
                  _ -> error $ "Failed to parse line: " <> T.unpack line
            loop

  putStrLn "["
  traverse_ (putStrLn . toNix) mavenNixs
  putStrLn "]"

parseRepos :: Element -> Map String String
parseRepos pom = Map.fromList $ do
  repoList <- findChildrenByTagName "repositories" pom
  repo <- findChildrenByTagName "repository" repoList
  repoId <- findChildrenByTagName "id" repo
  repoUrl <- findChildrenByTagName "url" repo
  return (strContent repoId, strContent repoUrl)

data Maven = Maven
  { _maven_groupId :: String
  , _maven_artifactId :: String
  , _maven_version :: String
  }
  deriving (Show, Read, Eq, Ord)

data MavenNix = MavenNix
  { _mavenNix_maven :: Maven
  , _mavenNix_repo :: String
  , _mavenNix_jarSha256 :: Maybe (Digest SHA256State)
  , _mavenNix_pomSha256 :: Maybe (Digest SHA256State)
  , _mavenNix_aarSha256 :: Maybe (Digest SHA256State)
  }
  deriving (Show, Eq, Ord)

-- | Create a nix record for a hashed maven package
toNix :: MavenNix -> String
toNix m =
  let mvn = _mavenNix_maven m
      showHash h = fromMaybe "null" $ (\x -> "\"" <> x <> "\"") . showDigest <$> h
  in unlines
      [ "  { artifactId = \"" <> _maven_artifactId mvn <> "\";"
      , "    groupId = \"" <> _maven_groupId mvn <> "\";"
      , "    version = \"" <> _maven_version mvn <> "\";"
      , "    repo = \"" <> _mavenNix_repo m <> "\";"
      , "    jarSha256 = " <> showHash (_mavenNix_jarSha256 m) <> ";"
      , "    pomSha256 = " <> showHash (_mavenNix_pomSha256 m) <> ";"
      , "    aarSha256 = " <> showHash (_mavenNix_aarSha256 m) <> "; }"
      ]

-- | Gets the repo with the given id, calling 'empty' when it's not present
getRepo :: (MonadReader HavenEnv m, MonadPlus m) => String -> m String
getRepo repoId = do
  repos <- asks _havenEnv_repos
  maybe empty pure $ Map.lookup repoId repos

m2Directory :: Maven -> String
m2Directory mvn = foldl (</>) ""
  [ (\x -> if x == '.' then '/' else x) <$> _maven_groupId mvn
  , _maven_artifactId mvn
  , _maven_version mvn
  ]

-- | Gets a given artifact for a 'Maven' and hashes it. It will first
-- check the local m2 dir, and then it will try to download it from
-- the online repo. If both fail, an error is logged to 'stderr', and
-- 'empty' is called.
getArtifactFile
  :: (MonadIO m, MonadPlus m, MonadReader HavenEnv m)
  => Maven
  -> String
  -> String
  -> m BL.ByteString
getArtifactFile mvn ext repo = do
  mgr <- asks _havenEnv_manager
  m2Repo <- asks _havenEnv_m2Local
  let m2Dir = m2Directory mvn
      m2Filename = _maven_artifactId mvn <> "-" <> _maven_version mvn <> ext
      path = m2Repo </> m2Dir </> m2Filename
  m2ArtifactExists <- liftIO $ doesFileExist path
  if m2ArtifactExists then liftIO (BL.readFile path) else do
    let url = repo </> m2Dir </> m2Filename
    req <- liftIO $ parseRequest url
    log $ "Getting URL: " <> T.pack url
    rsp <- liftIO $ httpLbs req mgr
    when (responseStatus rsp /= status200) $ do
      log $ "Failed to get URL: " <> T.pack url
      empty
    return $ responseBody rsp

-- | Hash a particular maven package's .pom and .jar files and parse the .pom file as xml
fetch
  :: (MonadIO m, MonadReader HavenEnv m)
  => String
  -> Maven
  -> MaybeT m [MavenNix]
fetch fileType mvn = do
  m2Repo <- asks _havenEnv_m2Local
  let m2Dir = m2Repo </> m2Directory mvn
      findRepoId = takeWhile (/='=') . drop 1 . dropWhile (/='>')

  repoId <- findRepoId <$> liftIO (readFile (m2Dir </> "_remote.repositories"))
  repo <- getRepo repoId

  pom <- runMaybeT $ getArtifactFile mvn ".pom" repo

  let noArtifacts = MavenNix
        { _mavenNix_maven = mvn
        , _mavenNix_repo = repo
        , _mavenNix_jarSha256 = Nothing
        , _mavenNix_pomSha256 = sha256 <$> pom
        , _mavenNix_aarSha256 = Nothing
        }

  parents <- fmap (fromMaybe []) $ for pom $ \pomContents -> do
    pomEl <- maybe empty pure $ parseXMLDoc pomContents
    fmap mconcat $ traverse (fetch "pom") $ do
      parent <- findChildrenByTagName "parent" pomEl
      groupId <- strContent <$> findChildrenByTagName "groupId" parent
      artifactId <- strContent <$> findChildrenByTagName "artifactId" parent
      version <- strContent <$> findChildrenByTagName "version" parent
      return $ Maven groupId artifactId version

  -- TODO: Match the 'type' to the correct file extension.
  -- The extension is _usually_ equal to the type, but it's not necessarily.
  -- See: https://maven.apache.org/pom.html#Dependencies
  mavenNix <- asum
    [ do
      guard (fileType == "jar")
      jarSha <- sha256 <$> getArtifactFile mvn ".jar" repo
      return $ noArtifacts { _mavenNix_jarSha256 = Just jarSha }
    , do
      guard (fileType == "aar")
      aarSha <- sha256 <$> getArtifactFile mvn ".aar" repo
      return $ noArtifacts { _mavenNix_aarSha256 = Just aarSha }
    , do
      guard (fileType == "pom") -- This is used when getting parents
      return noArtifacts
    ]
  return (mavenNix:parents)

-- | Retrieve an XML Element's children by tag name
findChildrenByTagName :: String -> Element -> [Element]
findChildrenByTagName n = filterChildren (\a -> qName (elName a) == n)

firstChildByTagName :: String -> Element -> Maybe Element
firstChildByTagName n = listToMaybe . findChildrenByTagName n
