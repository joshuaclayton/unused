module Unused.TermSearch
    ( SearchResults(..)
    , SearchBackend(..)
    , SearchTerm
    , search
    ) where

import qualified Data.Maybe as M
import           GHC.IO.Exception (ExitCode(ExitSuccess))
import qualified System.Process as P
import           Unused.TermSearch.Internal (commandLineOptions, parseSearchResult)
import           Unused.TermSearch.Types (SearchResults(..), SearchBackend(..))
import           Unused.Types (SearchTerm, searchTermToString)

search :: SearchBackend -> SearchTerm -> IO SearchResults
search backend t =
    SearchResults . M.mapMaybe (parseSearchResult backend t) <$> (lines <$> performSearch backend (searchTermToString t))

performSearch :: SearchBackend -> String -> IO String
performSearch b t = extractSearchResults b <$> searchOutcome
  where
    searchOutcome =
        P.readProcessWithExitCode
            (backendToCommand b)
            (commandLineOptions b t)
            ""
    backendToCommand Rg = "rg"
    backendToCommand Ag = "ag"

extractSearchResults :: SearchBackend -> (ExitCode, String, String) -> String
extractSearchResults Rg (ExitSuccess, stdout, _) = stdout
extractSearchResults Rg (_, _, stderr) = stderr
extractSearchResults Ag (_, stdout, _) = stdout
