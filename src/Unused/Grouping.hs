module Unused.Grouping
    ( Grouping(..)
    , CurrentGrouping(..)
    , GroupedTerms
    , groupedResponses
    ) where

import qualified Data.Map.Strict as Map
import Data.List (sort, nub)
import Unused.Types
import Unused.ResponseFilter (updateMatches)
import Unused.Grouping.Types
import Unused.Grouping.Internal

groupedResponses :: CurrentGrouping -> TermMatchSet -> [GroupedTerms]
groupedResponses g tms =
    (\g' -> (g', groupedMatchSetSubsets currentGroup g' tms)) <$> groupingsFromSet
  where
    groupingsFromSet = allGroupings currentGroup tms
    currentGroup = groupFilter g

groupedMatchSetSubsets :: GroupFilter -> Grouping -> TermMatchSet -> TermMatchSet
groupedMatchSetSubsets f tms =
    updateMatches newMatches
  where
    newMatches = filter ((== tms) . f)

allGroupings :: GroupFilter -> TermMatchSet -> [Grouping]
allGroupings f =
    uniqueValues . Map.map (fmap f . trMatches)
  where
    uniqueValues = sort . nub . concat . Map.elems
