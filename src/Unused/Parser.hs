module Unused.Parser
    ( parseResults
    ) where

import           Control.Arrow ((&&&))
import qualified Data.Bifunctor as BF
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import           Unused.Aliases (groupedTermsAndAliases)
import           Unused.LikelihoodCalculator (calculateLikelihood)
import           Unused.ResultsClassifier.Types (LanguageConfiguration(..))
import           Unused.TermSearch (SearchResults, fromResults)
import           Unused.Types (TermMatchSet, TermMatch, resultsFromMatches, tmDisplayTerm)

parseResults :: [LanguageConfiguration] -> SearchResults -> TermMatchSet
parseResults lcs =
    Map.fromList . map (BF.second $ calculateLikelihood lcs . resultsFromMatches) . groupResults . fromResults

groupResults :: [TermMatch] -> [(String, [TermMatch])]
groupResults ms =
    map (toKey &&& id) groupedMatches
  where
    toKey = L.intercalate "|" . L.nub . L.sort . map tmDisplayTerm
    groupedMatches = groupedTermsAndAliases ms
