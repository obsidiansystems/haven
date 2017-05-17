{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Digest.Pure.SHA
import Data.Foldable
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup
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
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

data HavenEnv = HavenEnv
  { _havenEnv_manager :: Manager
  , _havenEnv_repos :: Map String String
  , _havenEnv_m2Local :: FilePath
  }

-- | Takes multiple maven package descriptions as command line arguments
-- and finds the dependencies of those maven packages.
-- Package descriptions should be of the form @groupid:artifactid:version@
main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  [pomXml] <- getArgs
  Just repos <- fmap parseRepos . parseXMLDoc <$> BS.readFile pomXml

  withSystemTempFile "out.txt" $ \tmpFile hTmpFile -> withSystemTempDirectory "m2" $ \m2Repo -> do
    let havenEnv = HavenEnv mgr repos m2Repo
    hProc <- runProcess
      "mvn"
      ["-f", pomXml, "dependency:tree", "-Dverbose", "-DoutputFile=" <> tmpFile, "-Dmaven.repo.local=" <> m2Repo]
      Nothing
      Nothing
      Nothing
      (Just stderr)
      Nothing
    ExitSuccess <- waitForProcess hProc

    _ <- hGetLine hTmpFile -- skip local package name
    putStrLn "["
    fix $ \loop -> do
      e <- hIsEOF hTmpFile
      unless e $ do
        line <- hGetLine hTmpFile
        let mavenGrArTyVr = dropWhile (not . isAlphaNum) line -- Skip leading symbols; we don't care about parsing this
            (groupId, ':':mavenArTyVr) = break (==':') mavenGrArTyVr
            (artifactId, ':':mavenTyVr) = break (==':') mavenArTyVr
            (fileType, ':':mavenVr) = break (==':') mavenTyVr -- File type that Maven has decided on
            version = takeWhile (/=':') mavenVr -- Version number
            maven = Maven groupId artifactId version
        unless (all isSpace line) $ do
          mMvnNix <- runReaderT (fetch fileType maven) havenEnv
          case mMvnNix of
            Just mvnNix -> putStrLn $ toNix mvnNix
            Nothing -> do
              hPutStrLn stderr $ "Failed for " <> unlines [show fileType, show maven]
              exitFailure
          loop
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

getRepo :: (MonadReader HavenEnv m, MonadPlus m) => String -> m String
getRepo repoId = do
  repos <- asks _havenEnv_repos
  maybe empty pure $ Map.lookup repoId repos

-- | Hash a particular maven package's .pom and .jar files and parse the .pom file as xml
fetch
  :: (MonadIO m, MonadReader HavenEnv m)
  => String
  -> Maven
  -> m (Maybe MavenNix)
fetch fileType mvn = runMaybeT $ do
  m2Repo <- asks _havenEnv_m2Local
  let component = foldl (</>) ""
        [ (\x -> if x == '.' then '/' else x) <$> _maven_groupId mvn
        , _maven_artifactId mvn
        , _maven_version mvn
        ]
      m2Dir = m2Repo </> component
      m2Filename ext = _maven_artifactId mvn <> "-" <> _maven_version mvn <> ext
      findRepoId = takeWhile (/='=') . drop 1 . dropWhile (/='>')

  repoId <- findRepoId <$> liftIO (readFile (m2Dir </> "_remote.repositories"))
  repo <- getRepo repoId

  let shaArtifact ext = do
        mgr <- asks _havenEnv_manager
        let path = m2Dir </> m2Filename ext
        m2ArtifactExists <- liftIO $ doesFileExist path
        contents <- if m2ArtifactExists then liftIO (BL.readFile path) else do
          let url = repo </> component </> m2Filename ext
          req <- liftIO $ parseRequest url
          liftIO $ hPutStrLn stderr $ "Getting URL: " <> url
          rsp <- liftIO $ httpLbs req mgr
          when (responseStatus rsp /= status200) $ do
            liftIO $ hPutStrLn stderr $ "Failed to get URL: " <> url
            empty
          return $ responseBody rsp
        return $ sha256 contents

  pomSha <- runMaybeT $ shaArtifact ".pom"
  
  let noArtifacts = MavenNix
        { _mavenNix_maven = mvn
        , _mavenNix_repo = repo
        , _mavenNix_jarSha256 = Nothing
        , _mavenNix_pomSha256 = pomSha
        , _mavenNix_aarSha256 = Nothing
        }

  -- TODO: Match the 'type' to the correct file extension.
  -- The extension is _usually_ equal to the type, but it's not necessarily.
  -- See: https://maven.apache.org/pom.html#Dependencies
  asum
    [ do
      guard (fileType == "jar")
      jarSha <- shaArtifact ".jar"
      return $ noArtifacts { _mavenNix_jarSha256 = Just jarSha }
    , do
      guard (fileType == "aar")
      aarSha <- shaArtifact ".aar"
      return $ noArtifacts { _mavenNix_jarSha256 = Just aarSha }
    ]

-- | Retrieve an XML Element's children by tag name
findChildrenByTagName :: String -> Element -> [Element]
findChildrenByTagName n = filterChildren (\a -> qName (elName a) == n)

firstChildByTagName :: String -> Element -> Maybe Element
firstChildByTagName n = listToMaybe . findChildrenByTagName n
