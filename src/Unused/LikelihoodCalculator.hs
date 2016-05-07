module Unused.LikelihoodCalculator
    ( calculateLikelihood
    ) where

import Unused.Types (TermResults, RemovalLikelihood(..), trRemovalLikelihood, trTotalOccurrences)
import Unused.ResponseFilter (railsSingleOkay, elixirSingleOkay)

calculateLikelihood :: TermResults -> TermResults
calculateLikelihood r =
    r { trRemovalLikelihood = newLikelihood }
  where
    baseScore = trTotalOccurrences r
    railsScore = if railsSingleOkay r then 5 else 0
    elixirScore = if elixirSingleOkay r then 5 else 0
    totalScore = baseScore + railsScore + elixirScore
    newLikelihood
        | totalScore < 3 = High
        | totalScore < 6 = Medium
        | totalScore < 9 = Low
        | otherwise = Low
