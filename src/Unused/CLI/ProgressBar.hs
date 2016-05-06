module Unused.CLI.ProgressBar
    ( ProgressBar
    , startProgressBar
    , incrementProgressBar
    , stopProgressBar
    ) where

import Control.Concurrent (ThreadId, killThread)
import System.ProgressBar (ProgressRef, startProgress, incProgress, msg, percentage)

data ProgressBar = ProgressBar
    { pbProgressRef :: ProgressRef
    , pbThreadId :: ThreadId
    }

startProgressBar :: Int -> IO ProgressBar
startProgressBar i = do
    (ref, tid) <- buildProgressBar $ toInteger i
    return $ ProgressBar ref tid

incrementProgressBar :: ProgressBar -> IO ()
incrementProgressBar ProgressBar{ pbProgressRef = ref } =
    incProgress ref 1

stopProgressBar :: ProgressBar -> IO ()
stopProgressBar ProgressBar { pbThreadId = tid } = killThread tid

buildProgressBar :: Integer -> IO (ProgressRef, ThreadId)
buildProgressBar =
    startProgress (msg message) percentage progressBarWidth
  where
    message = "Working"
    progressBarWidth = 60
