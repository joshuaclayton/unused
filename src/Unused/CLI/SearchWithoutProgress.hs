module Unused.CLI.SearchWithoutProgress
    ( searchWithoutProgressBar
    ) where

import Unused.TermSearch (search)
import Unused.CLI.Spinner (startSpinner, stopSpinner)

searchWithoutProgressBar :: [String] -> IO [String]
searchWithoutProgressBar terms = do
    putStr " "
    sp <- startSpinner
    concat <$> mapM search terms <* stopSpinner sp
