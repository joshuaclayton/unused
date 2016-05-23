{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Unused.TermSearch.Types
    ( SearchResults(..)
    , fromResults
    ) where

import Unused.Types (TermMatch)

newtype SearchResults = SearchResults [TermMatch] deriving (Monoid)

fromResults :: SearchResults -> [TermMatch]
fromResults (SearchResults a) = a
