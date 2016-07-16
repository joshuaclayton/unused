module Unused.CLI.Search
    ( SearchRunner(..)
    , renderHeader
    , executeSearch
    ) where

import qualified Unused.CLI.ProgressIndicator as I
import qualified Unused.CLI.Util as U
import qualified Unused.CLI.Views as V
import qualified Unused.TermSearch as TS

data SearchRunner = SearchWithProgress | SearchWithoutProgress

renderHeader :: [a] -> IO ()
renderHeader terms = do
    U.resetScreen
    V.analysisHeader terms

executeSearch :: SearchRunner -> [String] -> IO TS.SearchResults
executeSearch runner terms = do
    renderHeader terms
    runSearch runner terms <* U.resetScreen

runSearch :: SearchRunner -> [String] -> IO TS.SearchResults
runSearch SearchWithProgress    = I.progressWithIndicator TS.search I.createProgressBar
runSearch SearchWithoutProgress = I.progressWithIndicator TS.search I.createSpinner
