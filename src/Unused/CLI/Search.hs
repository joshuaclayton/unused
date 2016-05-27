module Unused.CLI.Search
    ( SearchRunner(..)
    , renderHeader
    , executeSearch
    ) where

import Unused.TermSearch (SearchResults, search)
import Unused.CLI.Util
import qualified Unused.CLI.Views as V
import Unused.CLI.ProgressIndicator

data SearchRunner = SearchWithProgress | SearchWithoutProgress

renderHeader :: [String] -> IO ()
renderHeader terms = do
    resetScreen
    V.analysisHeader terms

executeSearch :: SearchRunner -> [String] -> IO SearchResults
executeSearch runner terms = do
    renderHeader terms
    runSearch runner terms <* resetScreen

runSearch :: SearchRunner -> [String] -> IO SearchResults
runSearch SearchWithProgress    = progressWithIndicator search createProgressBar
runSearch SearchWithoutProgress = progressWithIndicator search createSpinner
