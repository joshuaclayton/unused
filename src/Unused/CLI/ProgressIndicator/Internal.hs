module Unused.CLI.ProgressIndicator.Internal
    ( start
    , stop
    , increment
    , printPrefix
    ) where

import qualified Control.Concurrent as CC
import qualified Control.Monad as M
import qualified System.ProgressBar as PB
import           Unused.CLI.ProgressIndicator.Types (ProgressIndicator(..))
import           Unused.CLI.Util

start :: ProgressIndicator -> Int -> IO (CC.ThreadId, ProgressIndicator)
start s@Spinner{} _ = do
    tid <- CC.forkIO $ runSpinner 0 s
    return (tid, s { sThreadId = Just tid })
start ProgressBar{} i = do
    (ref, tid) <- buildProgressBar $ toInteger i
    return (tid, ProgressBar (Just ref) (Just tid))

stop :: ProgressIndicator -> IO ()
stop ProgressBar{ pbThreadId = Just tid } = CC.killThread tid
stop Spinner{ sThreadId = Just tid } = CC.killThread tid
stop _ = return ()

increment :: ProgressIndicator -> IO ()
increment ProgressBar{ pbProgressRef = Just ref } = PB.incProgress ref 1
increment _ = return ()

printPrefix :: ProgressIndicator -> IO ()
printPrefix ProgressBar{} = putStr "\n\n"
printPrefix Spinner{} = putStr " "

runSpinner :: Int -> ProgressIndicator -> IO ()
runSpinner i s@Spinner{ sDelay = delay, sSnapshots = snapshots, sColors = colors, sLength = length' } = M.forever $ do
    setSGR [SetColor Foreground Dull currentColor]
    putStr currentSnapshot
    cursorBackward 1
    CC.threadDelay delay
    runSpinner (i + 1) s
  where
    currentSnapshot = snapshots !! (i `mod` snapshotLength)
    currentColor = colors !! (i `div` snapshotLength)
    snapshotLength = length'
runSpinner _ _ = return ()

buildProgressBar :: Integer -> IO (PB.ProgressRef, CC.ThreadId)
buildProgressBar =
    PB.startProgress (PB.msg message) PB.percentage progressBarWidth
  where
    message = "Working"
    progressBarWidth = 60
