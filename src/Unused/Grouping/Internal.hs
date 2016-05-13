module Unused.Grouping.Internal
    ( groupFilter
    ) where

import Unused.Grouping.Types
import System.FilePath (takeDirectory, splitDirectories)
import Unused.Types (tmPath, tmTerm)
import Data.List (intercalate)

groupFilter :: CurrentGrouping -> GroupFilter
groupFilter GroupByDirectory = fileNameGrouping
groupFilter GroupByTerm = termGrouping
groupFilter GroupByFile = fileGrouping
groupFilter NoGroup = const NoGrouping

fileNameGrouping :: GroupFilter
fileNameGrouping = ByDirectory . shortenedDirectory . tmPath

termGrouping :: GroupFilter
termGrouping = ByTerm . tmTerm

fileGrouping :: GroupFilter
fileGrouping = ByFile . tmPath

shortenedDirectory :: String -> String
shortenedDirectory =
    intercalate "/" . take 2 . splitDirectories . takeDirectory
