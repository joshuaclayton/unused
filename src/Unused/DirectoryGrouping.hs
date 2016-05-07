module Unused.DirectoryGrouping
    ( DirectoryPrefix(..)
    , responsesGroupedByPath
    ) where

import System.FilePath (takeDirectory, splitDirectories)
import qualified Data.Map.Strict as Map
import Data.List (intercalate, sort, nub)
import Unused.Types
import Unused.ResponseFilter (updateMatches)

newtype DirectoryPrefix = DirectoryPrefix String deriving (Eq, Show, Ord)

responsesGroupedByPath :: TermMatchSet -> [(DirectoryPrefix, TermMatchSet)]
responsesGroupedByPath pr =
    (\p -> (p, responseForPath p pr)) <$> directoriesForGrouping pr

responseForPath :: DirectoryPrefix -> TermMatchSet -> TermMatchSet
responseForPath s =
    updateMatches newMatches
  where
    newMatches = filter ((== s) . fileNameGrouping . tmPath)

fileNameGrouping :: String -> DirectoryPrefix
fileNameGrouping =
    DirectoryPrefix . grouping
  where
    grouping = intercalate "/" . take 2 . splitDirectories . takeDirectory

directoriesForGrouping :: TermMatchSet -> [DirectoryPrefix]
directoriesForGrouping =
    uniqueValues . Map.map (fmap (fileNameGrouping . tmPath) . trMatches)
  where
    uniqueValues = sort . nub . concat . Map.elems
