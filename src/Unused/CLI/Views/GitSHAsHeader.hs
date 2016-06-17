module Unused.CLI.Views.GitSHAsHeader
    ( loadingSHAsHeader
    ) where

import Unused.CLI.Util

loadingSHAsHeader :: Int -> IO ()
loadingSHAsHeader commitCount = do
    setSGR [SetConsoleIntensity BoldIntensity]
    putStr "Unused: "
    setSGR [Reset]

    putStr "loading the most recent "

    setSGR [SetColor Foreground Dull Green]
    putStr $ show commitCount
    setSGR [Reset]
    putStr " SHAs from git"
