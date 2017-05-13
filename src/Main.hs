{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.State
import Data.Digest.Pure.SHA
import Data.List
import Data.Maybe
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import System.Environment
import Text.XML.Light

-- | Takes multiple maven package descriptions as command line arguments
-- and finds the dependencies of those maven packages.
-- Package descriptions should be of the form @groupid:artifactid:version@
main :: IO ()
main = do
  mavens <- getArgs
  mgr <- newManager tlsManagerSettings
  (_, deps) <- flip runStateT Set.empty $ traverse (recurseDependencies mgr) $ parseMaven <$> mavens
  putStrLn $ "[" <> (concatMap toNix deps)  <> "]"


data Maven = Maven
  { _maven_groupId :: String
  , _maven_artifactId :: String
  , _maven_version :: String
  }
  deriving (Show, Read, Eq, Ord)

data MavenNix = MavenNix
  { _mavenNix_maven :: Maven
  , _mavenNix_jarSha256 :: Maybe (Digest SHA256State)
  , _mavenNix_pomSha256 :: Maybe (Digest SHA256State)
  }
  deriving (Show, Eq, Ord)

-- | Parses strings of the form @groupid:artifactid:version@,
-- e.g., @com.android.build.tools:gradle:2.3.0@
parseMaven :: String -> Maven
parseMaven s =
  let (groupId, _:rest) = break (==':') s
      (artifactId, _:version) = break (==':') rest
  in Maven groupId artifactId version

-- | Create a nix record for a hashed maven package
toNix :: MavenNix -> String
toNix m =
  let mvn = _mavenNix_maven m
      showHash h = fromMaybe "null" $ (\x -> "\"" <> x <> "\"") . showDigest <$> h
  in unlines
      [ "  { artifactId = \"" <> _maven_artifactId mvn <> "\";"
      , "    groupId = \"" <> _maven_groupId mvn <> "\";"
      , "    version = \"" <> _maven_version mvn <> "\";"
      , "    jarSha256 = " <> showHash (_mavenNix_jarSha256 m) <> ";"
      , "    pomSha256 = " <> showHash (_mavenNix_pomSha256 m) <> "; }"
      ]

-- | Maven repositories in which to look for packages
mirrors :: [String]
mirrors =
  [ "https://repo1.maven.org/maven2/"
  , "https://jcenter.bintray.com/"
  , "http://central.maven.org/maven2/"
  ]

-- | Hash a particular maven package's .pom and .jar files and parse the .pom file as xml
fetch :: [String] -> Manager -> Maven -> IO (MavenNix, Element)
fetch (mirror:fallbacks) mgr mvn = do
  let version = takeWhile (/= ',') $ dropWhile (\c -> c == '[' || c == '(') $ _maven_version mvn
      url ext = mconcat
        [ mirror
        , (\x -> if x == '.' then '/' else x) <$> _maven_groupId mvn
        , "/"
        , _maven_artifactId mvn
        , "/"
        , version
        , "/"
        , _maven_artifactId mvn
        , "-"
        , version
        , ext
        ]
      hash rsp = if responseStatus rsp == status200
                   then Just $ sha256 $ responseBody rsp
                   else Nothing
  reqPom <- parseRequest $ url ".pom"
  reqJar <- parseRequest $ url ".jar"
  pom <- httpLbs reqPom mgr
  jar <- httpLbs reqJar mgr
  if responseStatus jar /= status200 && responseStatus pom /= status200
    then fetch fallbacks mgr mvn
    else return $
      let Just e = parseXMLDoc $ responseBody pom
          mvnNix = MavenNix
            { _mavenNix_maven = mvn
            , _mavenNix_jarSha256 = hash jar
            , _mavenNix_pomSha256 = hash pom
            }
      in (mvnNix, e)
fetch [] _ mvn = error $ mconcat
  [ "Error: Could not find "
  , _maven_groupId mvn
  , ":"
  , _maven_artifactId mvn
  , ":"
  , _maven_version mvn
  , " in any mirror."
  ]

-- | Extract the dependencies from a package's pom xml
getDepsFor :: Element -> [Maven]
getDepsFor x = do
  deps <- findChildrenByTagName "dependencies" x
  dep <- findChildrenByTagName "dependency" deps
  groupId <- findChildrenByTagName "groupId" dep
  artifactId <- findChildrenByTagName "artifactId" dep
  version <- findChildrenByTagName "version" dep
  let optional = case findChildrenByTagName "optional" dep of
        [] -> False
        xs -> any ((=="true") . strContent) xs
  guard (not ("$" `isPrefixOf` strContent version) && not optional)
  return $ Maven
    { _maven_groupId = strContent groupId
    , _maven_artifactId = strContent artifactId
    , _maven_version = strContent version
    }

-- | Given a starting maven package, retrieve its dependencies recursively
recurseDependencies :: Manager -> Maven -> StateT (Set MavenNix) IO ()
recurseDependencies mgr mvn = do
  s <- get
  when (not $ any (\mvnNix -> _mavenNix_maven mvnNix == mvn) s) $ do
    (mvnNix, e) <- liftIO $ fetch mirrors mgr mvn
    modify $ Set.insert mvnNix
    void $ traverse (recurseDependencies mgr) $ getDepsFor e

-- | Retrieve an XML Element's children by tag name
findChildrenByTagName :: String -> Element -> [Element]
findChildrenByTagName n = filterChildren (\a -> qName (elName a) == n)
