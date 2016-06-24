module Unused.CLI.Views.InvalidConfigError
    ( invalidConfigError
    ) where

import Unused.CLI.Util
import Unused.CLI.Views.Error
import Unused.ResultsClassifier (ParseConfigError(..))

invalidConfigError :: [ParseConfigError] -> IO ()
invalidConfigError es = do
    errorHeader "There was a problem with the following config file(s):"

    mapM_ configError es

    setSGR [Reset]

configError :: ParseConfigError -> IO ()
configError ParseConfigError{ pcePath = path, pceParseError = msg} = do
    setSGR [SetConsoleIntensity BoldIntensity]
    putStrLn path
    setSGR [Reset]
    putStrLn $ "    " ++ msg
