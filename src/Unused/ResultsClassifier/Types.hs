{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Unused.ResultsClassifier.Types
    ( LanguageConfiguration(..)
    , LowLikelihoodMatch(..)
    , TermAlias(..)
    , Position(..)
    , Matcher(..)
    , ParseConfigError(..)
    ) where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Yaml (FromJSON(..), (.:), (.:?), (.!=))
import qualified Data.Yaml as Y
import           Unused.Projection

data LanguageConfiguration = LanguageConfiguration
    { lcName :: String
    , lcAllowedTerms :: [String]
    , lcAutoLowLikelihood :: [LowLikelihoodMatch]
    , lcTermAliases :: [TermAlias]
    }

data LowLikelihoodMatch = LowLikelihoodMatch
    { smName :: String
    , smMatchers :: [Matcher]
    , smClassOrModule :: Bool
    }

data TermAlias = TermAlias
    { taFrom :: String
    , taTo :: String
    , taTransform :: Text -> Text
    }

data ParseConfigError = ParseConfigError
    { pcePath :: String
    , pceParseError :: String
    }

data Position = StartsWith | EndsWith | Equals
data Matcher = Term Position String | Path Position String | AppOccurrences Int | AllowedTerms [String]

instance FromJSON LanguageConfiguration where
    parseJSON (Y.Object o) = LanguageConfiguration
        <$> o .: "name"
        <*> o .:? "allowedTerms" .!= []
        <*> o .:? "autoLowLikelihood" .!= []
        <*> o .:? "aliases" .!= []
    parseJSON _ = M.mzero

instance FromJSON LowLikelihoodMatch where
    parseJSON (Y.Object o) = LowLikelihoodMatch
        <$> o .: "name"
        <*> parseMatchers o
        <*> o .:? "classOrModule" .!= False
    parseJSON _ = M.mzero

instance FromJSON TermAlias where
    parseJSON (Y.Object o) = TermAlias
        <$> o .: "from"
        <*> o .: "to"
        <*> (either (fail . show) return =<< (translate . T.pack <$> (o .: "to")))
    parseJSON _ = M.mzero

data MatchHandler a = MatchHandler
    { mhKeys :: [String]
    , mhKeyToMatcher :: T.Text -> Either T.Text (a -> Matcher)
    }

intHandler :: MatchHandler Int
intHandler = MatchHandler
    { mhKeys = ["appOccurrences"]
    , mhKeyToMatcher = keyToMatcher
    }
  where
    keyToMatcher "appOccurrences" = Right AppOccurrences
    keyToMatcher t                = Left t

stringHandler :: MatchHandler String
stringHandler = MatchHandler
    { mhKeys = ["pathStartsWith", "pathEndsWith", "termStartsWith", "termEndsWith", "termEquals"]
    , mhKeyToMatcher = keyToMatcher
    }
  where
    keyToMatcher "pathStartsWith" = Right $ Path StartsWith
    keyToMatcher "pathEndsWith"   = Right $ Path EndsWith
    keyToMatcher "termStartsWith" = Right $ Term StartsWith
    keyToMatcher "termEndsWith"   = Right $ Term EndsWith
    keyToMatcher "termEquals"     = Right $ Term Equals
    keyToMatcher t                = Left t

stringListHandler :: MatchHandler [String]
stringListHandler = MatchHandler
    { mhKeys = ["allowedTerms"]
    , mhKeyToMatcher = keyToMatcher
    }
  where
    keyToMatcher "allowedTerms" = Right AllowedTerms
    keyToMatcher t              = Left t

lowLikelihoodMatchKeys :: [T.Text]
lowLikelihoodMatchKeys =
    map T.pack $ ["name", "classOrModule"] ++ mhKeys intHandler ++ mhKeys stringHandler ++ mhKeys stringListHandler

validateLowLikelihoodKeys :: Y.Object -> Y.Parser [Matcher] -> Y.Parser [Matcher]
validateLowLikelihoodKeys o ms =
    if fullOverlap
        then ms
        else fail $ "The following keys are unsupported: " ++ L.intercalate ", " (T.unpack <$> unsupportedKeys)
  where
    fullOverlap = null unsupportedKeys
    unsupportedKeys = HM.keys o L.\\ lowLikelihoodMatchKeys

parseMatchers :: Y.Object -> Y.Parser [Matcher]
parseMatchers o =
    validateLowLikelihoodKeys o $ myFold (++) [buildMatcherList o intHandler, buildMatcherList o stringHandler, buildMatcherList o stringListHandler]
  where
    myFold :: (Foldable t, Monad m) => (a -> a -> a) -> t (m a) -> m a
    myFold f = foldl1 (\acc i -> acc >>= (\l -> f l <$> i))

buildMatcherList :: FromJSON a => Y.Object -> MatchHandler a -> Y.Parser [Matcher]
buildMatcherList o mh =
    sequenceA $ matcherParserForKey <$> keysToParse
  where
    matcherParserForKey k = extractMatcher (mhKeyToMatcher mh k) $ mKey k
    keysToParse = positionKeysforMatcher o (mhKeys mh)
    mKey = (.:?) o

positionKeysforMatcher :: Y.Object -> [String] -> [T.Text]
positionKeysforMatcher o ls = L.intersect (T.pack <$> ls) $ HM.keys o

extractMatcher :: Either T.Text (a -> Matcher) -> Y.Parser (Maybe a) -> Y.Parser Matcher
extractMatcher e p = either displayFailure (convertFoundObjectToMatcher p) e

convertFoundObjectToMatcher :: (Monad m, A.Alternative m) => m (Maybe a) -> (a -> b) -> m b
convertFoundObjectToMatcher p f = maybe A.empty (pure . f) =<< p

displayFailure :: T.Text -> Y.Parser a
displayFailure t = fail $ "Parse error: '" ++ T.unpack t ++ "' is not a valid key in a singleOnly matcher"
