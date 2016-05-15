module Unused.CLI.Search
    ( SearchRunner(..)
    , renderHeader
    , executeSearch
    ) where

import Unused.TermSearch (search)
import Unused.CLI.Util
import Unused.CLI.ProgressIndicator

data SearchRunner = SearchWithProgress | SearchWithoutProgress

renderHeader :: [String] -> IO ()
renderHeader terms = do
    resetScreen
    hideCursor
    printAnalysisHeader terms

executeSearch :: SearchRunner -> [String] -> IO [String]
executeSearch runner terms = do
    renderHeader terms
    runSearch runner terms <* resetScreen <* showCursor

printAnalysisHeader :: [String] -> IO ()
printAnalysisHeader terms = do
    setSGR [SetConsoleIntensity BoldIntensity]
    putStr "Unused: "
    setSGR [Reset]

    putStr "analyzing "

    setSGR [SetColor Foreground Dull Green]
    putStr $ show $ length terms
    setSGR [Reset]
    putStr " terms"

runSearch :: SearchRunner -> [String] -> IO [String]
runSearch SearchWithProgress    = progressWithIndicator search createProgressBar
runSearch SearchWithoutProgress = progressWithIndicator search createSpinner
