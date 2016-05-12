module Unused.CLI.Util
    ( resetScreen
    , withInterruptHandler
    , module System.Console.ANSI
    ) where

import Control.Monad (void)
import System.Console.ANSI
import Control.Exception (throwTo)
import System.Posix.Signals (Handler(Catch), installHandler, keyboardSignal)
import Control.Concurrent (ThreadId, myThreadId)
import System.Exit (ExitCode(ExitFailure))

resetScreen :: IO ()
resetScreen = do
    clearScreen
    setCursorPosition 0 0

withInterruptHandler :: IO () -> IO ()
withInterruptHandler body = do
    tid <- myThreadId
    void $ installHandler keyboardSignal (Catch (handleInterrupt tid)) Nothing
    body

handleInterrupt :: ThreadId -> IO ()
handleInterrupt tid = do
    resetScreen
    showCursor
    throwTo tid $ ExitFailure code
  where
    code = signalToInt $ 128 + keyboardSignal
    signalToInt s = read $ show s :: Int
