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

parseLines :: String -> ParseResponse
parseLines =
    responseFromParse . parse parseTermMatches "matches"

responseFromParse :: Either ParseError [TermMatch] -> ParseResponse
responseFromParse =
    fmap $ Map.fromList . map (second $ calculateLikelihood . resultsFromMatches) . groupBy tmTerm
