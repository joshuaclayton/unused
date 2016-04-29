module Unused.Types
    ( TermMatch(..)
    ) where

data TermMatch = TermMatch
    { term :: String
    , path :: String
    , occurrences :: Int
    } deriving Show
