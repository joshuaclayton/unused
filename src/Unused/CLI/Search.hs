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

executeSearch :: TS.SearchBackend -> SearchRunner -> [TS.SearchTerm] -> IO TS.SearchResults
executeSearch backend runner terms = do
    renderHeader terms
    runSearch backend runner terms <* U.resetScreen

runSearch :: TS.SearchBackend -> SearchRunner -> [TS.SearchTerm] -> IO TS.SearchResults
runSearch b SearchWithProgress    = I.progressWithIndicator (TS.search b) I.createProgressBar
runSearch b SearchWithoutProgress = I.progressWithIndicator (TS.search b) I.createSpinner
