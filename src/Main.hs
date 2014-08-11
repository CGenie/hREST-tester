{-# LANGUAGE OverloadedStrings #-}

module Main
where

import Control.Applicative
import Control.Lens
import Control.Monad (mzero)
import Data.Aeson.Lens
import Data.ByteString as B
import Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Yaml as Y
import Data.Text
import Data.Text.Encoding as E
import qualified Data.Text.Encoding as StrictText


-- START Data.Aeson.Lens hacking
-- (see https://github.com/ekmett/lens/commit/2bb06c634f73110ff84ce0113be9ad4ac66bcb6d#diff-d41d8cd98f00b204e9800998ecf8427e)
instance AsNumber Text

instance AsPrimitive Text


instance AsValue Text where
    _Value = strictTextUtf8._JSON
    {-# INLINE _Value #-}


strictTextUtf8 :: Iso' Text B.ByteString
strictTextUtf8 = iso StrictText.encodeUtf8 StrictText.decodeUtf8
-- END Data.Aeson.Lens hacking


data RequestMethod = GET | POST | PUT | DELETE
    deriving (Show)

type URL = Text
type ParamKey = Text
type ParamValue = Text


data ScenarioSettings = ScenarioSettings {
    method     :: Text
  , url        :: Text
  , parameters :: Map.Map Text Text
} deriving (Show)

instance Y.FromJSON ScenarioSettings where
    parseJSON (Y.Object v) = ScenarioSettings <$>
                           v Y..:  "method" <*>
                           v Y..:  "url" <*>
                           v Y..:? "parameters" Y..!= (Map.fromList [])
    parseJSON _          = mzero


data Scenario = Scenario {
    scenarioRequestMethod :: RequestMethod
  , scenarioUrl           :: URL
  , scenarioParams        :: Map.Map ParamKey ParamValue
  , scenarioDependencies  :: [Scenario]
} deriving (Show)


getMethod :: Text -> Maybe RequestMethod
getMethod "GET"    = Just GET
getMethod "POST"   = Just POST
getMethod "PUT"    = Just PUT
getMethod "DELETE" = Just DELETE
getMethod _        = Nothing


parseScenario :: ScenarioSettings -> Maybe Scenario
parseScenario ss = do
    methodY <- getMethod $ method ss

    Just Scenario { scenarioRequestMethod = methodY,
                    scenarioUrl = url ss,
                    scenarioParams = parameters ss,
                    scenarioDependencies = [] }


readYAMLFile :: FilePath -> IO ScenarioSettings
readYAMLFile fileName = do
    str <- B.readFile fileName
    case Y.decode str :: Maybe ScenarioSettings of
        Nothing   -> error "Failed to parse"
        Just yaml -> return yaml


main :: IO ()
main = do
    yaml <- readYAMLFile "scenario.yaml"

    let s = parseScenario yaml

    print $ show s

