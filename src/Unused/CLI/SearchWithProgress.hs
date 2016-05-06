module Unused.CLI.SearchWithProgress
    ( searchWithProgressBar
    ) where

import Unused.CLI.ProgressBar (ProgressBar, startProgressBar, incrementProgressBar, stopProgressBar)
import Unused.TermSearch (search)

searchWithProgressBar :: [String] -> IO [String]
searchWithProgressBar terms = do
    putStr "\n\n"
    bar <- startProgressBar $ length terms
    concat <$> mapM (performSearch bar) terms <* stopProgressBar bar

performSearch :: ProgressBar -> String -> IO [String]
performSearch bar t =
    search t <* incrementProgressBar bar
