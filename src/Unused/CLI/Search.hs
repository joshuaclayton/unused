module Unused.CLI.Search
    ( SearchRunner(..)
    , executeSearch
    ) where

import Unused.CLI.SearchWithProgress (searchWithProgressBar)
import Unused.CLI.SearchWithoutProgress (searchWithoutProgressBar)
import Unused.CLI.Util

data SearchRunner = SearchWithProgress | SearchWithoutProgress

executeSearch :: SearchRunner -> [String] -> IO [String]
executeSearch runner terms = do
    resetScreen
    hideCursor
    printAnalysisHeader terms
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

runSearch :: SearchRunner -> ([String] -> IO [String])
runSearch SearchWithProgress    = searchWithProgressBar
runSearch SearchWithoutProgress = searchWithoutProgressBar
