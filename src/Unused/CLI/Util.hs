module Unused.CLI.Util
    ( resetScreen
    , withRuntime
    , installChildInterruptHandler
    , module System.Console.ANSI
    ) where

import Control.Concurrent.ParallelIO
import Control.Monad (void)
import System.Console.ANSI
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Control.Exception (throwTo)
import System.Posix.Signals (Handler(Catch), installHandler, keyboardSignal)
import Control.Concurrent (ThreadId, myThreadId, killThread)
import System.Exit (ExitCode(ExitFailure))

withRuntime :: IO a -> IO a
withRuntime a = do
    hSetBuffering stdout NoBuffering
    withInterruptHandler $ withoutCursor a <* stopGlobalPool

resetScreen :: IO ()
resetScreen = do
    clearScreen
    setCursorPosition 0 0

withoutCursor :: IO a -> IO a
withoutCursor body = do
    hideCursor
    body <* showCursor

withInterruptHandler :: IO a -> IO a
withInterruptHandler body = do
    tid <- myThreadId
    void $ installHandler keyboardSignal (Catch (handleInterrupt tid)) Nothing
    body

installChildInterruptHandler :: ThreadId -> IO ()
installChildInterruptHandler tid = do
    currentThread <- myThreadId
    void $ installHandler keyboardSignal (Catch (handleChildInterrupt currentThread tid)) Nothing

handleInterrupt :: ThreadId -> IO ()
handleInterrupt tid = do
    resetScreenState
    throwTo tid $ ExitFailure interruptExitCode

handleChildInterrupt :: ThreadId -> ThreadId -> IO ()
handleChildInterrupt parentTid childTid = do
    killThread childTid
    resetScreenState
    throwTo parentTid $ ExitFailure interruptExitCode
    handleInterrupt parentTid

interruptExitCode :: Int
interruptExitCode =
    signalToInt $ 128 + keyboardSignal
  where
    signalToInt s = read $ show s :: Int

resetScreenState :: IO ()
resetScreenState = do
    resetScreen
    showCursor
    setSGR [Reset]
