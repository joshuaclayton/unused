module Unused.CLI.Views.SearchResult.Internal
    ( termColor
    , removalReason
    ) where

import Unused.CLI.Util (Color(..))
import Unused.Types (TermResults(..), Removal(..), RemovalLikelihood(..))

termColor :: TermResults -> Color
termColor = likelihoodColor . rLikelihood . trRemoval

removalReason :: TermResults -> String
removalReason = rReason . trRemoval

likelihoodColor :: RemovalLikelihood -> Color
likelihoodColor High = Red
likelihoodColor Medium = Yellow
likelihoodColor Low = Green
likelihoodColor Unknown = Black
likelihoodColor NotCalculated = Magenta
