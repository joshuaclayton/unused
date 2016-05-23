module Unused.Parser
    ( parseLines
    , ParseError
    ) where

import Data.Bifunctor (second)
import qualified Data.Map.Strict as Map
import Unused.Util (groupBy)
import Unused.Types (ParseResponse, TermMatchSet, TermMatch, resultsFromMatches, tmTerm)
import Unused.LikelihoodCalculator
import Unused.Parser.Internal

parseLines :: [LanguageConfiguration] -> String -> ParseResponse
parseLines lcs =
    fmap (matchesToMatchSet lcs) . parse parseTermMatches "matches"

matchesToMatchSet :: [LanguageConfiguration] -> [TermMatch] -> TermMatchSet
matchesToMatchSet lcs =
    Map.fromList . map (second $ calculateLikelihood lcs . resultsFromMatches) . groupBy tmTerm
