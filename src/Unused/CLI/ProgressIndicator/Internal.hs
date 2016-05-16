module Unused.CLI.ProgressIndicator.Internal
    ( start
    , stop
    , increment
    , printPrefix
    ) where

import Control.Monad (forever)
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import System.ProgressBar (ProgressRef, startProgress, incProgress, msg, percentage)
import Unused.CLI.ProgressIndicator.Types
import Unused.CLI.Util

start :: ProgressIndicator -> Int -> IO (ThreadId, ProgressIndicator)
start s@Spinner{} _ = do
    tid <- forkIO $ runSpinner 0 s
    return (tid, s { sThreadId = Just tid })
start ProgressBar{} i = do
    (ref, tid) <- buildProgressBar $ toInteger i
    return (tid, ProgressBar (Just ref) (Just tid))

stop :: ProgressIndicator -> IO ()
stop ProgressBar{ pbThreadId = Just tid } = killThread tid
stop Spinner{ sThreadId = Just tid } = killThread tid
stop _ = return ()

increment :: ProgressIndicator -> IO ()
increment ProgressBar{ pbProgressRef = Just ref } = incProgress ref 1
increment _ = return ()

printPrefix :: ProgressIndicator -> IO ()
printPrefix ProgressBar{} = putStr "\n\n"
printPrefix Spinner{} = putStr " "

runSpinner :: Int -> ProgressIndicator -> IO ()
runSpinner i s@Spinner{ sDelay = delay, sSnapshots = snapshots, sColors = colors, sLength = length' } = forever $ do
    setSGR [SetColor Foreground Dull currentColor]
    putStr currentSnapshot
    cursorBackward 1
    threadDelay delay
    runSpinner (i + 1) s
  where
    currentSnapshot = snapshots !! (i `mod` snapshotLength)
    currentColor = colors !! (i `div` snapshotLength)
    snapshotLength = length'
runSpinner _ _ = return ()

buildProgressBar :: Integer -> IO (ProgressRef, ThreadId)
buildProgressBar =
    startProgress (msg message) percentage progressBarWidth
  where
    message = "Working"
    progressBarWidth = 60
