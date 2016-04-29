module Unused.Types
    ( TermMatch(..)
    , TermMatchSet
    , ParseResponse(..)
    ) where

import Text.Parsec (ParseError)
import Data.Map.Strict as Map

data TermMatch = TermMatch
    { term :: String
    , path :: String
    , occurrences :: Int
    } deriving Show

type TermMatchSet = Map String [TermMatch]

data ParseResponse = ValidParse TermMatchSet | InvalidParse ParseError
