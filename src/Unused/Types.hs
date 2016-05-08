module Unused.Types
    ( TermMatch(..)
    , TermResults(..)
    , TermMatchSet
    , ParseResponse
    , RemovalLikelihood(..)
    , resultsFromMatches
    ) where

import Text.Parsec (ParseError)
import qualified Data.Map.Strict as Map

data TermMatch = TermMatch
    { tmTerm :: String
    , tmPath :: String
    , tmOccurrences :: Int
    } deriving (Eq, Show)

data TermResults = TermResults
    { trTerm :: String
    , trMatches :: [TermMatch]
    , trTotalFiles :: Int
    , trTotalOccurrences :: Int
    , trRemovalLikelihood :: RemovalLikelihood
    } deriving (Eq, Show)

data RemovalLikelihood = High | Medium | Low | Unknown deriving (Eq, Show)

type TermMatchSet = Map.Map String TermResults

type ParseResponse = Either ParseError TermMatchSet

resultsFromMatches :: [TermMatch] -> TermResults
resultsFromMatches m =
    TermResults
        { trTerm = resultTerm terms
        , trMatches = m
        , trTotalFiles = totalFiles
        , trTotalOccurrences = totalOccurrences
        , trRemovalLikelihood = Unknown
        }
  where
    totalFiles = length m
    totalOccurrences = sum $ fmap tmOccurrences m
    terms = map tmTerm m
    resultTerm (x:_) = x
    resultTerm _ = ""
