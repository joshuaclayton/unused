module Unused.CLI.Views.FingerprintError
    ( fingerprintError
    ) where

import Data.List (intercalate)
import Unused.Cache.DirectoryFingerprint
import Unused.CLI.Views.Error

fingerprintError :: FingerprintOutcome -> IO ()
fingerprintError e = do
    errorHeader "There was a problem generating a cache fingerprint:"

    printOutcomeMessage e

printOutcomeMessage :: FingerprintOutcome -> IO ()
printOutcomeMessage (MD5ExecutableNotFound execs) =
    putStrLn $
        "Unable to find any of the following executables \
        \in your PATH: " ++ intercalate ", " execs
