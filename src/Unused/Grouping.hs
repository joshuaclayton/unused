module Unused.Grouping
    ( Grouping(..)
    , CurrentGrouping(..)
    , GroupedTerms
    , groupedResponses
    ) where

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import           Unused.Grouping.Internal (groupFilter)
import           Unused.Grouping.Types (Grouping(..), CurrentGrouping(..), GroupFilter, GroupedTerms)
import           Unused.ResponseFilter (updateMatches)
import           Unused.Types (TermMatchSet, TermResults(trMatches))

groupedResponses :: CurrentGrouping -> TermMatchSet -> [GroupedTerms]
groupedResponses g tms =
    (\g' -> (g', groupedMatchSetSubsets currentGroup g' tms)) <$> groupingsFromSet
  where
    groupingsFromSet = allGroupings currentGroup tms
    currentGroup = groupFilter g

groupedMatchSetSubsets :: GroupFilter -> Grouping -> TermMatchSet -> TermMatchSet
groupedMatchSetSubsets f tms =
    updateMatches $ filter ((== tms) . f)

allGroupings :: GroupFilter -> TermMatchSet -> [Grouping]
allGroupings f =
    uniqueValues . Map.map (fmap f . trMatches)
  where
    uniqueValues = L.sort . L.nub . concat . Map.elems
