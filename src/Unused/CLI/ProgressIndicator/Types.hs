module Unused.CLI.ProgressIndicator.Types
    ( ProgressIndicator(..)
    ) where

import Control.Concurrent (ThreadId)
import System.ProgressBar (ProgressRef)
import System.Console.ANSI (Color)

data ProgressIndicator
    = Spinner
        { sSnapshots :: [String]
        , sLength :: Int
        , sDelay :: Int
        , sColors :: [Color]
        , sThreadId :: Maybe ThreadId
        }
    | ProgressBar
        { pbProgressRef :: Maybe ProgressRef
        , pbThreadId :: Maybe ThreadId
        }
