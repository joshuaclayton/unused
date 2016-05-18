module Unused.Parser
    ( parseLines
    , ParseError
    ) where

import Data.Bifunctor (second)
import qualified Data.Map.Strict as Map
import Unused.Util (groupBy)
import Unused.Types (ParseResponse, TermMatch, resultsFromMatches, tmTerm)
import Unused.LikelihoodCalculator
import Unused.Parser.Internal

parseLines :: [LanguageConfiguration] -> String -> ParseResponse
parseLines lcs =
    responseFromParse lcs . parse parseTermMatches "matches"

responseFromParse :: [LanguageConfiguration] -> Either ParseError [TermMatch] -> ParseResponse
responseFromParse lcs =
    fmap $ Map.fromList . map (second $ calculateLikelihood lcs . resultsFromMatches) . groupBy tmTerm
