module Unused.CLI.GitContext
    ( loadGitContext
    ) where

import qualified Data.Map.Strict as Map
import Unused.CLI.ProgressIndicator
       (createProgressBar, progressWithIndicator)
import qualified Unused.CLI.Util as U
import qualified Unused.CLI.Views as V
import Unused.GitContext (gitContextForResults)
import Unused.Types (TermMatchSet)

loadGitContext :: Int -> TermMatchSet -> IO TermMatchSet
loadGitContext i tms = do
    U.resetScreen
    V.loadingSHAsHeader i
    Map.fromList <$> progressWithIndicator (gitContextForResults i) createProgressBar listTerms
  where
    listTerms = Map.toList tms
