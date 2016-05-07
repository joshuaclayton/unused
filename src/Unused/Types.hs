module Unused.Types
    ( TermMatch(..)
    , TermResults(..)
    , TermMatchSet
    , ParseResponse
    , RemovalLikelihood(..)
    ) where

import Text.Parsec (ParseError)
import qualified Data.Map.Strict as Map

data TermMatch = TermMatch
    { tmTerm :: String
    , tmPath :: String
    , tmOccurrences :: Int
    } deriving Show

data TermResults = TermResults
    { trTerm :: String
    , trMatches :: [TermMatch]
    , trTotalFiles :: Int
    , trTotalOccurrences :: Int
    , trRemovalLikelihood :: RemovalLikelihood
    } deriving Show

data RemovalLikelihood = High | Medium | Low deriving Show

type TermMatchSet = Map.Map String TermResults

type ParseResponse = Either ParseError TermMatchSet
