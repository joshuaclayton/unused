{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Unused.ResultsClassifier.Types
    ( LanguageConfiguration(..)
    , LowLikelihoodMatch(..)
    , Position(..)
    , Matcher(..)
    ) where

import Control.Monad (mzero)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified Data.List as L
import Data.HashMap.Strict (keys)
import Control.Applicative (Alternative, empty)
import Data.Yaml (FromJSON(..), (.:), (.:?), (.!=))

data LanguageConfiguration = LanguageConfiguration
    { lcName :: String
    , lcAllowedTerms :: [String]
    , lcAutoLowLikelihood :: [LowLikelihoodMatch]
    } deriving Show

data LowLikelihoodMatch = LowLikelihoodMatch
    { smName :: String
    , smMatchers :: [Matcher]
    , smClassOrModule :: Bool
    } deriving Show

data Position = StartsWith | EndsWith deriving Show
data Matcher = Term Position String | Path Position String | AppOccurrences Int deriving Show

instance FromJSON LanguageConfiguration where
    parseJSON (Y.Object o) = LanguageConfiguration
        <$> o .: "name"
        <*> o .: "allowedTerms"
        <*> o .: "autoLowLikelihood"
    parseJSON _ = mzero

instance FromJSON LowLikelihoodMatch where
    parseJSON (Y.Object o) = LowLikelihoodMatch
        <$> o .: "name"
        <*> parseMatchers o
        <*> o .:? "classOrModule" .!= False
    parseJSON _ = mzero

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
    { mhKeys = ["pathStartsWith", "pathEndsWith", "termStartsWith", "termEndsWith"]
    , mhKeyToMatcher = keyToMatcher
    }
  where
    keyToMatcher "pathStartsWith" = Right $ Path StartsWith
    keyToMatcher "pathEndsWith"   = Right $ Path EndsWith
    keyToMatcher "termStartsWith" = Right $ Term StartsWith
    keyToMatcher "termEndsWith"   = Right $ Term EndsWith
    keyToMatcher t                = Left t

parseMatchers :: Y.Object -> Y.Parser [Matcher]
parseMatchers o =
    myFold (++) [buildMatcherList o intHandler, buildMatcherList o stringHandler]
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
positionKeysforMatcher o ls = L.intersect (T.pack <$> ls) $ keys o

extractMatcher :: Either T.Text (a -> Matcher) -> Y.Parser (Maybe a) -> Y.Parser Matcher
extractMatcher e p = either displayFailure (convertFoundObjectToMatcher p) e

convertFoundObjectToMatcher :: (Monad m, Alternative m) => m (Maybe a) -> (a -> b) -> m b
convertFoundObjectToMatcher p f = maybe empty (pure . f) =<< p

displayFailure :: T.Text -> a
displayFailure t = error $ "Parse error: '" ++ T.unpack t ++ "' is not a valid key in a singleOnly matcher"