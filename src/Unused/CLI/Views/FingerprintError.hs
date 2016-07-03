module Unused.CLI.Views.FingerprintError
    ( fingerprintError
    ) where

import qualified Data.List as L
import qualified Unused.CLI.Views.Error as V
import           Unused.Cache.DirectoryFingerprint (FingerprintOutcome(..))

fingerprintError :: FingerprintOutcome -> IO ()
fingerprintError e = do
    V.errorHeader "There was a problem generating a cache fingerprint:"

    printOutcomeMessage e

printOutcomeMessage :: FingerprintOutcome -> IO ()
printOutcomeMessage (MD5ExecutableNotFound execs) =
    putStrLn $
        "Unable to find any of the following executables \
        \in your PATH: " ++ L.intercalate ", " execs
