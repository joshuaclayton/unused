module Unused.CLI.Util
    ( resetScreen
    , module System.Console.ANSI
    ) where

import System.Console.ANSI

resetScreen :: IO ()
resetScreen = do
    clearScreen
    setCursorPosition 0 0
