module Unused.Parser
    ( parseResults
    ) where

import Data.Bifunctor (second)
import Control.Arrow ((&&&))
import qualified Data.Map.Strict as Map
import Data.List (intercalate, sort, nub)
import Unused.TermSearch (SearchResults, fromResults)
import Unused.Types (TermMatchSet, TermMatch, resultsFromMatches, tmTerm)
import Unused.LikelihoodCalculator
import Unused.ResultsClassifier.Types
import Unused.Aliases

parseResults :: [LanguageConfiguration] -> SearchResults -> TermMatchSet
parseResults lcs =
    Map.fromList . map (second $ calculateLikelihood lcs . resultsFromMatches) . groupResults aliases . fromResults
  where
    aliases = concatMap lcTermAliases lcs

groupResults :: [TermAlias] -> [TermMatch] -> [(String, [TermMatch])]
groupResults aliases ms =
    map (toKey &&& id) groupedMatches
  where
    toKey = intercalate "|" . nub . sort . map tmTerm
    groupedMatches = groupedTermsAndAliases aliases ms
