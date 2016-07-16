module Unused.CLI.Views.AnalysisHeader
    ( analysisHeader
    ) where

import Unused.CLI.Util

analysisHeader :: [a] -> IO ()
analysisHeader terms = do
    setSGR [SetConsoleIntensity BoldIntensity]
    putStr "Unused: "
    setSGR [Reset]

    putStr "analyzing "

    setSGR [SetColor Foreground Dull Green]
    putStr $ show $ length terms
    setSGR [Reset]
    putStr " terms"
