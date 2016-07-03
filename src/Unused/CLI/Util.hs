module Unused.CLI.Util
    ( resetScreen
    , withRuntime
    , installChildInterruptHandler
    , module System.Console.ANSI
    ) where

import qualified Control.Concurrent as CC
import qualified Control.Concurrent.ParallelIO as PIO
import qualified Control.Exception as E
import qualified Control.Monad as M
import           System.Console.ANSI
import qualified System.Exit as Ex
import           System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import qualified System.Posix.Signals as S

withRuntime :: IO a -> IO a
withRuntime a = do
    hSetBuffering stdout NoBuffering
    withInterruptHandler $ withoutCursor a <* PIO.stopGlobalPool

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
    tid <- CC.myThreadId
    M.void $ S.installHandler S.keyboardSignal (S.Catch (handleInterrupt tid)) Nothing
    body

installChildInterruptHandler :: CC.ThreadId -> IO ()
installChildInterruptHandler tid = do
    currentThread <- CC.myThreadId
    M.void $ S.installHandler S.keyboardSignal (S.Catch (handleChildInterrupt currentThread tid)) Nothing

handleInterrupt :: CC.ThreadId -> IO ()
handleInterrupt tid = do
    resetScreenState
    E.throwTo tid $ Ex.ExitFailure interruptExitCode

handleChildInterrupt :: CC.ThreadId -> CC.ThreadId -> IO ()
handleChildInterrupt parentTid childTid = do
    CC.killThread childTid
    resetScreenState
    E.throwTo parentTid $ Ex.ExitFailure interruptExitCode
    handleInterrupt parentTid

interruptExitCode :: Int
interruptExitCode =
    signalToInt $ 128 + S.keyboardSignal
  where
    signalToInt s = read $ show s :: Int

resetScreenState :: IO ()
resetScreenState = do
    resetScreen
    showCursor
    setSGR [Reset]
