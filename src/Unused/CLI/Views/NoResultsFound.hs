module Unused.CLI.Views.NoResultsFound
    ( noResultsFound
    ) where

import Unused.CLI.Util

noResultsFound :: IO ()
noResultsFound = do
    setSGR [SetColor Foreground Dull Green]
    setSGR [SetConsoleIntensity BoldIntensity]
    putStrLn "Unused found no results"
    setSGR [Reset]
