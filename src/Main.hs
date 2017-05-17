{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
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
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process
import Text.XML.Light

-- | Takes multiple maven package descriptions as command line arguments
-- and finds the dependencies of those maven packages.
-- Package descriptions should be of the form @groupid:artifactid:version@
main :: IO ()
main = do
  [pomXml] <- getArgs
  Just repos <- fmap parseRepos . parseXMLDoc <$> BS.readFile pomXml

  withSystemTempFile "pom.xml" $ \tmpFile hTmpFile -> withSystemTempDirectory "m2" $ \m2Repo -> do
    hProc <- runProcess
      "mvn"
      ["-f", pomXml, "dependency:tree", "-DoutputFile=" <> tmpFile, "-Dmaven.repo.local=" <> m2Repo]
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
          Just mvnNix <- fetch m2Repo fileType repos maven -- fileType should be used here to only fetch needed files
          putStrLn $ toNix mvnNix
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

-- | Hash a particular maven package's .pom and .jar files and parse the .pom file as xml
fetch :: FilePath -> String -> Map String String -> Maven -> IO (Maybe MavenNix)
fetch m2Repo fileType repos mvn = do
  let m2Dir = foldl (</>) m2Repo
        [ (\x -> if x == '.' then '/' else x) <$> _maven_groupId mvn
        , _maven_artifactId mvn
        , _maven_version mvn
        ]
      m2File ext = m2Dir </> _maven_artifactId mvn <> "-" <> _maven_version mvn <> ext
      findRepoId = takeWhile (/='=') . drop 1 . dropWhile (/='>')

  repoId <- findRepoId <$> readFile (m2Dir </> "_remote.repositories")

  let pomFile = m2File ".pom"
      jarFile = m2File ".jar"
      aarFile = m2File ".aar"

  pomExists <- doesFileExist pomFile -- Not all repos have poms, though this seems to be exceptionally rare
  pomSha <- if pomExists then Just . sha256 <$> BL.readFile pomFile else return Nothing

  runMaybeT $ do
    repo <- maybe empty pure $ Map.lookup repoId repos

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
        jarSha <- sha256 <$> liftIO (BL.readFile jarFile)
        return $ noArtifacts { _mavenNix_jarSha256 = Just jarSha }
      , do
        guard (fileType == "aar")
        aarSha <- sha256 <$> liftIO (BL.readFile aarFile)
        return $ noArtifacts { _mavenNix_jarSha256 = Just aarSha }
      ]

-- | Retrieve an XML Element's children by tag name
findChildrenByTagName :: String -> Element -> [Element]
findChildrenByTagName n = filterChildren (\a -> qName (elName a) == n)

firstChildByTagName :: String -> Element -> Maybe Element
firstChildByTagName n = listToMaybe . findChildrenByTagName n
