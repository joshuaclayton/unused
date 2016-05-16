module Unused.CLI.Util
    ( resetScreen
    , withoutCursor
    , withInterruptHandler
    , module System.Console.ANSI
    ) where

import Control.Monad (void)
import System.Console.ANSI
import Control.Exception (throwTo)
import System.Posix.Signals (Handler(Catch), installHandler, keyboardSignal)
import Control.Concurrent (ThreadId, myThreadId)
import System.Exit (ExitCode(ExitFailure))

withoutCursor :: IO a -> IO a
withoutCursor body = do
    hideCursor
    body <* showCursor

resetScreen :: IO ()
resetScreen = do
    clearScreen
    setCursorPosition 0 0

withInterruptHandler :: IO a -> IO a
withInterruptHandler body = do
    tid <- myThreadId
    void $ installHandler keyboardSignal (Catch (handleInterrupt tid)) Nothing
    body

handleInterrupt :: ThreadId -> IO ()
handleInterrupt tid = do
    resetScreen
    showCursor
    setSGR [Reset]
    throwTo tid $ ExitFailure interruptExitCode

interruptExitCode :: Int
interruptExitCode =
    signalToInt $ 128 + keyboardSignal
  where
    signalToInt s = read $ show s :: Int
