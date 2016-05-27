module Unused.CLI.Views.SearchResult.ColumnFormatter
    ( ColumnFormat(..)
    , buildColumnFormatter
    ) where

import Text.Printf
import Unused.Types (TermResults(..), TermMatch(..), totalFileCount, totalOccurrenceCount)

data ColumnFormat = ColumnFormat
    { cfPrintTerm :: String -> String
    , cfPrintPath :: String -> String
    , cfPrintNumber :: Int -> String
    }

buildColumnFormatter :: [TermResults] -> ColumnFormat
buildColumnFormatter r =
    ColumnFormat (printf $ termFormat r) (printf $ pathFormat r) (printf $ numberFormat r)

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

numberFormat :: [TermResults] -> String
numberFormat rs =
    "%" ++ show numberWidth ++ "d"
  where
    numberWidth = maximum [fileWidth, occurrenceWidth]
    fileWidth = maximum $ fileLength =<< rs
    occurrenceWidth = maximum $ occurrenceLength =<< rs
    fileLength = return . numberLength . totalFileCount
    occurrenceLength = return . numberLength . totalOccurrenceCount

numberLength :: Int -> Int
numberLength i =
    1 + floor (logBase 10 $ fromIntegral i :: Double)
