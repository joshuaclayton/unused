module Unused.CLI.Views.InvalidConfigError
    ( invalidConfigError
    ) where

import           Unused.CLI.Util
import qualified Unused.CLI.Views.Error as V
import           Unused.ResultsClassifier (ParseConfigError(..))

invalidConfigError :: [ParseConfigError] -> IO ()
invalidConfigError es = do
    V.errorHeader "There was a problem with the following config file(s):"

    mapM_ configError es

    setSGR [Reset]

configError :: ParseConfigError -> IO ()
configError ParseConfigError{ pcePath = path, pceParseError = msg} = do
    setSGR [SetConsoleIntensity BoldIntensity]
    putStrLn path
    setSGR [Reset]
    putStrLn $ "    " ++ msg
