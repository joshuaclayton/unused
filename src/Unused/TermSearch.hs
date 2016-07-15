module Unused.TermSearch
    ( SearchResults(..)
    , SearchTerm
    , search
    ) where

import qualified Data.Maybe as M
import qualified System.Process as P
import           Unused.TermSearch.Internal (commandLineOptions, parseSearchResult)
import           Unused.TermSearch.Types (SearchResults(..))
import           Unused.Types (SearchTerm, searchTermToString)

search :: SearchTerm -> IO SearchResults
search t =
    SearchResults . M.mapMaybe (parseSearchResult t) <$> (lines <$> ag (searchTermToString t))

ag :: String -> IO String
ag t = do
  (_, results, _) <- P.readProcessWithExitCode "ag" (commandLineOptions t) ""
  return results
