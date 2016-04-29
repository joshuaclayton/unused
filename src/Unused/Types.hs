module Unused.Types
    ( TermMatch(..)
    , TermMatchSet
    , ParseResponse(..)
    , responseFromParse
    , withOneFile
    , withOneOccurrence
    ) where

import Text.Parsec (ParseError)
import qualified Data.Map.Strict as Map
import Data.List (isInfixOf)
import Unused.Util (groupBy)

data TermMatch = TermMatch
    { term :: String
    , path :: String
    , occurrences :: Int
    } deriving Show

type TermMatchSet = Map.Map String [TermMatch]

type ParseResponse = Either ParseError TermMatchSet

responseFromParse :: Either ParseError [TermMatch] -> ParseResponse
responseFromParse =
    fmap $ Map.fromList . groupBy term

withOneFile :: ParseResponse -> ParseResponse
withOneFile = fmap $ Map.filterWithKey (\_ a -> length a == 1)

withOneOccurrence :: ParseResponse -> ParseResponse
withOneOccurrence = fmap $ Map.filterWithKey (\_ a -> (sum $ fmap occurrences a) == 1)

notMatchingPath :: String -> ParseResponse -> ParseResponse
notMatchingPath s =
    fmap $ Map.map $ filter (not . isInfixOf s . path)
