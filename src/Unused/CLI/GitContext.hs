module Unused.CLI.GitContext
    ( loadGitContext
    ) where

import Data.Map.Strict as Map (toList, fromList)
import Unused.Types (TermMatchSet)
import Unused.CLI.Util
import qualified Unused.CLI.Views as V
import Unused.CLI.ProgressIndicator
import Unused.GitContext

loadGitContext :: Int -> TermMatchSet -> IO TermMatchSet
loadGitContext i tms = do
    resetScreen
    V.loadingSHAsHeader i
    Map.fromList <$> progressWithIndicator (gitContextForResults i) createProgressBar listTerms
  where
    listTerms = Map.toList tms
