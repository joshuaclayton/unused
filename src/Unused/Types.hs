module Unused.Types
    ( TermMatch(..)
    , TermMatchSet
    , ParseResponse(..)
    , listFromMatchSet
    , withOneFile
    , withOneOccurrence
    , resultsFromMatches
    ) where

import Text.Parsec (ParseError)
import Data.Bifunctor (second)
import qualified Data.Map.Strict as Map

data TermMatch = TermMatch
    { term :: String
    , path :: String
    , occurrences :: Int
    } deriving Show

data TermResults = TermResults
    { matches :: [TermMatch]
    , totalFiles :: Int
    , totalOccurrences :: Int
    }

type TermMatchSet = Map.Map String TermResults

type ParseResponse = Either ParseError TermMatchSet

resultsFromMatches :: [TermMatch] -> TermResults
resultsFromMatches m =
    TermResults
        { matches = m
        , totalFiles = totalFiles'
        , totalOccurrences = totalOccurrences'
        }
  where
    totalFiles' = length m
    totalOccurrences' = sum $ fmap occurrences m

withOneFile :: ParseResponse -> ParseResponse
withOneFile = fmap $ Map.filterWithKey (\_ a -> totalFiles a == 1)

withOneOccurrence :: ParseResponse -> ParseResponse
withOneOccurrence = fmap $ Map.filterWithKey (\_ a -> totalOccurrences a == 1)

listFromMatchSet :: TermMatchSet -> [(String, [TermMatch])]
listFromMatchSet =
  map (second matches) . Map.toList
