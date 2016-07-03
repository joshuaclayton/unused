module Unused.Parser
    ( parseResults
    ) where

import           Control.Arrow ((&&&))
import qualified Data.Bifunctor as BF
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import           Unused.Aliases (groupedTermsAndAliases)
import           Unused.LikelihoodCalculator (calculateLikelihood)
import           Unused.ResultsClassifier.Types (LanguageConfiguration(..), TermAlias)
import           Unused.TermSearch (SearchResults, fromResults)
import           Unused.Types (TermMatchSet, TermMatch, resultsFromMatches, tmTerm)

parseResults :: [LanguageConfiguration] -> SearchResults -> TermMatchSet
parseResults lcs =
    Map.fromList . map (BF.second $ calculateLikelihood lcs . resultsFromMatches) . groupResults aliases . fromResults
  where
    aliases = concatMap lcTermAliases lcs

groupResults :: [TermAlias] -> [TermMatch] -> [(String, [TermMatch])]
groupResults aliases ms =
    map (toKey &&& id) groupedMatches
  where
    toKey = L.intercalate "|" . L.nub . L.sort . map tmTerm
    groupedMatches = groupedTermsAndAliases aliases ms
