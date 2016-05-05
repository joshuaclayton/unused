module Unused.CLI.SearchWithoutProgress
    ( searchWithoutProgressBar
    ) where

import Unused.TermSearch (search)

searchWithoutProgressBar :: [String] -> IO [String]
searchWithoutProgressBar terms =
    concat <$> mapM search terms
