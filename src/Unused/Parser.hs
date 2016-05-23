module Unused.Parser
    ( parseResults
    ) where

import Data.Bifunctor (second)
import qualified Data.Map.Strict as Map
import Unused.Util (groupBy)
import Unused.TermSearch (SearchResults, fromResults)
import Unused.Types (TermMatchSet, resultsFromMatches, tmTerm)
import Unused.LikelihoodCalculator

parseResults :: [LanguageConfiguration] -> SearchResults -> TermMatchSet
parseResults lcs =
    Map.fromList . map (second $ calculateLikelihood lcs . resultsFromMatches) . groupBy tmTerm . fromResults
