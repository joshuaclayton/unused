module Unused.Grouping.Internal
    ( groupFilter
    ) where

import qualified Data.List as L
import qualified System.FilePath as FP
import           Unused.Grouping.Types (CurrentGrouping(..), Grouping(..), GroupFilter)
import qualified Unused.Types as T

groupFilter :: CurrentGrouping -> GroupFilter
groupFilter GroupByDirectory = ByDirectory . shortenedDirectory . T.tmPath
groupFilter GroupByTerm = ByTerm . T.tmTerm
groupFilter GroupByFile = ByFile . T.tmPath
groupFilter NoGroup = const NoGrouping

shortenedDirectory :: String -> String
shortenedDirectory =
    L.intercalate "/" . take 2 . FP.splitDirectories . FP.takeDirectory
