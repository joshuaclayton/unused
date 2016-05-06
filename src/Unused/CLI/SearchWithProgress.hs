module Unused.CLI.SearchWithProgress
    ( searchWithProgressBar
    ) where

import Control.Concurrent (ThreadId, killThread)
import System.ProgressBar (ProgressRef, startProgress, incProgress, msg, percentage)
import Unused.TermSearch (search)

searchWithProgressBar :: [String] -> IO [String]
searchWithProgressBar terms = do
    putStr "\n\n"
    (bar, tid) <- buildProgressBar $ toInteger $ length terms
    concat <$> mapM (performSearch bar) terms <* killThread tid

performSearch :: ProgressRef -> String -> IO [String]
performSearch ref t =
    search t <* incProgress ref 1

buildProgressBar :: Integer -> IO (ProgressRef, ThreadId)
buildProgressBar =
    startProgress (msg message) percentage progressBarWidth
  where
    message = "Working"
    progressBarWidth = 60
