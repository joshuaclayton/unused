module Unused.CLI.ProgressIndicator.Types
    ( ProgressIndicator(..)
    ) where

import qualified Control.Concurrent as CC
import qualified System.Console.ANSI as ANSI
import qualified System.ProgressBar as PB

data ProgressIndicator
    = Spinner { sSnapshots :: [String]
              , sLength :: Int
              , sDelay :: Int
              , sColors :: [ANSI.Color]
              , sThreadId :: Maybe CC.ThreadId }
    | ProgressBar { pbProgressRef :: Maybe PB.ProgressRef
                  , pbThreadId :: Maybe CC.ThreadId }
