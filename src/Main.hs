{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.State
import Data.Digest.Pure.SHA
import Data.Maybe
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Network.HTTP.Conduit hiding (path)
import Network.HTTP.Types.Status
import System.Environment
import Text.XML.Light

import Debug.Trace

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
  , _mavenNix_repo :: String
  , _mavenNix_jarSha256 :: Maybe (Digest SHA256State)
  , _mavenNix_pomSha256 :: Maybe (Digest SHA256State)
  , _mavenNix_aarSha256 :: Maybe (Digest SHA256State)
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
      , "    repo = \"" <> _mavenNix_repo m <> "\";"
      , "    jarSha256 = " <> showHash (_mavenNix_jarSha256 m) <> ";"
      , "    pomSha256 = " <> showHash (_mavenNix_pomSha256 m) <> ";"
      , "    aarSha256 = " <> showHash (_mavenNix_aarSha256 m) <> "; }"
      ]

-- | Maven repositories in which to look for packages
mirrors :: [String]
mirrors =
  [ "https://repo1.maven.org/maven2/"
  , "https://jcenter.bintray.com/"
  , "http://central.maven.org/maven2/"
  , "https://maven.atlassian.com/3rdparty/"
  ]

-- | Hash a particular maven package's .pom and .jar files and parse the .pom file as xml
fetch :: [String] -> Manager -> Maven -> IO (Maybe (MavenNix, Element))
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
  reqPom <- parseRequest $ trace (show $ url ".pom") $ url ".pom"
  reqJar <- parseRequest $ url ".jar"
  reqAar <- parseRequest $ url ".aar"
  pom <- httpLbs reqPom mgr
  jar <- httpLbs reqJar mgr
  aar <- httpLbs reqAar mgr
  if responseStatus jar /= status200 && responseStatus pom /= status200 && responseStatus aar /= status200
    then fetch fallbacks mgr mvn
    else return $
      let Just e = parseXMLDoc $ responseBody pom
          mvnNix = MavenNix
            { _mavenNix_maven = mvn
            , _mavenNix_repo = mirror
            , _mavenNix_jarSha256 = hash jar
            , _mavenNix_pomSha256 = hash pom
            , _mavenNix_aarSha256 = hash aar
            }
      in Just (mvnNix, e)
fetch [] _ mvn = trace (show $ mconcat
  [ "Error: Could not find "
  , _maven_groupId mvn
  , ":"
  , _maven_artifactId mvn
  , ":"
  , _maven_version mvn
  , " in any mirror."
  ]) $ return Nothing

-- | XML pom files may reference values defined elsewhere in the XML
-- using the syntax @${toplevelnode.childnode.grandchildnode}@.
-- They may also refer to values in the @properties@ node
strContentLookup :: Element -> String -> Maybe String
strContentLookup e path = case path of
  ('$':'{':rest) ->
    let path' = takeWhile (/='}') rest
        segments = words $ fmap (\c -> if c == '.' then ' ' else c) path'
        lookupSegment :: Maybe Element -> String -> Maybe Element
        lookupSegment parent seg = case parent of
          Just el -> firstChildByTagName seg el
          Nothing -> Nothing
        lookupSegments = foldl lookupSegment (Just e)
    in
      fmap strContent $ case segments of
        ("project":segs) -> case lookupSegments segments of
          Just str -> Just str
          -- The interpolator seems to fall back to looking things
          -- up in the @parent@ tag if they are not found as direct
          -- children of the project tag
          Nothing -> case lookupSegments $ "parent":segs of
            Just str -> Just str
            -- The interpolator also looks things up in the @properties
            -- tag, but in a slightly different way. The properties tag
            -- is only one level deep and the keys of its children
            -- are of the form @x.y.z@
            Nothing -> do
              props <- firstChildByTagName "properties" e
              prop <- firstChildByTagName path' props
              return prop
        _ -> lookupSegments segments
  _ -> Nothing

strContentWithInterpolation :: Element -> Element -> Maybe String
strContentWithInterpolation pom e = case strContent e of
  path@('$':_) -> strContentLookup pom path
  str -> Just str

-- | Extract maven package information from an XML element
-- (usually in a @dependency@ tag)
mavenFromXml :: Element -> Element -> [Maven]
mavenFromXml pom dep = catMaybes $ do
  groupId <- findChildrenByTagName "groupId" dep
  artifactId <- findChildrenByTagName "artifactId" dep
  version <- findChildrenByTagName "version" dep
  let optional = case findChildrenByTagName "optional" dep of
        [] -> False
        xs -> any ((=="true") . strContent) xs
  guard $ not optional
  return $ do
    g <- strContentWithInterpolation pom groupId
    a <- strContentWithInterpolation pom artifactId
    v <- strContentWithInterpolation pom version
    return $ Maven g a v

-- | Extract the dependencies from a package's pom xml
getDepsFor :: Element -> [Maven]
getDepsFor x = do
  deps <- findChildrenByTagName "dependencies" x
  dep <- findChildrenByTagName "dependency" deps
  mavenFromXml x dep

-- | Extract parent maven packages from a package's pom xml
getParentsFor :: Element -> [Maven]
getParentsFor x = do
  parents <- findChildrenByTagName "parent" x
  mavenFromXml x parents

-- getSubmodulesFor :: Element -> [Maven]
-- getSubmodulesFor x = do

-- | Given a starting maven package, retrieve its dependencies recursively
recurseDependencies :: Manager -> Maven -> StateT (Set MavenNix) IO ()
recurseDependencies mgr mvn = do
  s <- get
  when (not $ any (\mvnNix -> _mavenNix_maven mvnNix == mvn) s) $ do
    x <- liftIO $ fetch mirrors mgr mvn
    case x of
      Nothing -> return ()
      Just (mvnNix, e) -> do
        modify $ Set.insert mvnNix
        -- liftIO $ putStrLn $ "Deps for " <> _maven_artifactId mvn
        -- liftIO $ print (getDepsFor e <> getParentsFor e)
        void $ traverse (recurseDependencies mgr) $
          getDepsFor e <> getParentsFor e

-- | Retrieve an XML Element's children by tag name
findChildrenByTagName :: String -> Element -> [Element]
findChildrenByTagName n = filterChildren (\a -> qName (elName a) == n)

firstChildByTagName :: String -> Element -> Maybe Element
firstChildByTagName n = listToMaybe . findChildrenByTagName n
