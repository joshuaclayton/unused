module Unused.Grouping.Types
    ( Grouping(..)
    , CurrentGrouping(..)
    , GroupedTerms
    , GroupFilter
    ) where

import Unused.Types (TermMatch, TermMatchSet)

data Grouping
    = ByDirectory String
    | ByTerm String
    | ByFile String
    | NoGrouping
    deriving (Eq, Ord)

data CurrentGrouping
    = GroupByDirectory
    | GroupByTerm
    | GroupByFile
    | NoGroup

type GroupedTerms = (Grouping, TermMatchSet)

type GroupFilter = TermMatch -> Grouping

instance Show Grouping where
    show (ByDirectory s) = s
    show (ByTerm s) = s
    show (ByFile s) = s
    show NoGrouping = ""
