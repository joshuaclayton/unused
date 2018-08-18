{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Unused.TermSearch.Types
    ( SearchResults(..)
    , SearchBackend(..)
    ) where

import Unused.Types (TermMatch)

data SearchBackend
    = Ag
    | Rg

newtype SearchResults = SearchResults
    { fromResults :: [TermMatch]
    } deriving (Semigroup, Monoid)
