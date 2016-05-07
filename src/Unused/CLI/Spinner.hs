module Unused.CLI.Spinner
    ( startSpinner
    , stopSpinner
    ) where

import Control.Monad (forever)
import Control.Concurrent (ThreadId, killThread, threadDelay, forkIO)
import Unused.CLI.Util

data Spinner = Spinner
    { sSnapshots :: [String]
    , sLength :: Int
    , sDelay :: Int
    , sColors :: [Color]
    , sThreadId :: Maybe ThreadId
    }

startSpinner :: IO Spinner
startSpinner = do
    let s = buildSpinner
    tid <- forkIO $ runSpinner 0 s
    return $ s { sThreadId = Just tid }

stopSpinner :: Spinner -> IO ()
stopSpinner Spinner{ sThreadId = Nothing } = return ()
stopSpinner Spinner{ sThreadId = Just tid } = killThread tid

buildSpinner :: Spinner
buildSpinner =
    Spinner snapshots (length snapshots) 75000 colors Nothing
  where
    snapshots = ["⣾", "⣽", "⣻", "⢿", "⡿", "⣟", "⣯", "⣷"]
    colors = cycle [Black, Red, Yellow, Green, Blue, Cyan, Magenta]

runSpinner :: Int -> Spinner -> IO ()
runSpinner i s = forever $ do
    setSGR [SetColor Foreground Dull currentColor]
    putStr currentSnapshot
    cursorBackward 1
    threadDelay $ sDelay s
    runSpinner (i + 1) s
  where
    currentSnapshot = sSnapshots s !! (i `mod` snapshotLength)
    currentColor = sColors s !! (i `div` snapshotLength)
    snapshotLength = sLength s
