{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Unused.TermSearch.Types
    ( SearchResults(..)
    ) where

import Unused.Types (TermMatch)

newtype SearchResults = SearchResults { fromResults :: [TermMatch] } deriving (Monoid)
