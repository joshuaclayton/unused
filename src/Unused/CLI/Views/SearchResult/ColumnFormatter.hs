module Unused.CLI.Views.SearchResult.ColumnFormatter
    ( ColumnFormat(..)
    , buildColumnFormatter
    ) where

import Text.Printf (printf)
import Unused.Types (TermResults(..), TermMatch(..))

data ColumnFormat = ColumnFormat
    { cfPrintTerm :: String -> String
    , cfPrintPath :: String -> String
    }

buildColumnFormatter :: [TermResults] -> ColumnFormat
buildColumnFormatter r =
    ColumnFormat (printf $ termFormat r) (printf $ pathFormat r)

termFormat :: [TermResults] -> String
termFormat rs =
    "%-" ++ show termWidth ++ "s"
  where
    termWidth = maximum $ termLength =<< trMatches =<< rs
    termLength = return . length . tmTerm

pathFormat :: [TermResults] -> String
pathFormat rs =
    "%-" ++ show pathWidth ++ "s"
  where
    pathWidth = maximum $ pathLength =<< trMatches =<< rs
    pathLength = return . length . tmPath
