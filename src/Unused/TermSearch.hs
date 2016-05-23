module Unused.TermSearch
    ( SearchResults(..)
    , fromResults
    , search
    ) where

import System.Process
import Data.Maybe (mapMaybe)
import Unused.TermSearch.Types
import Unused.TermSearch.Internal

search :: String -> IO SearchResults
search t = do
    results <- lines <$> ag t
    return $ SearchResults $ mapMaybe (parseSearchResult t) results

ag :: String -> IO String
ag t = do
  (_, results, _) <- readProcessWithExitCode "ag" (commandLineOptions t) ""
  return results
