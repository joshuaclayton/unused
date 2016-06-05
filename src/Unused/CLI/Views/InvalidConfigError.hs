module Unused.CLI.Views.InvalidConfigError
    ( invalidConfigError
    ) where

import Unused.CLI.Util
import Unused.ResultsClassifier (ParseConfigError(..))

invalidConfigError :: [ParseConfigError] -> IO ()
invalidConfigError es = do
    setSGR [SetColor Background Vivid Red]
    setSGR [SetColor Foreground Vivid White]
    setSGR [SetConsoleIntensity BoldIntensity]

    putStrLn "\nThere was a problem with the following config file(s):\n"

    setSGR [Reset]

    mapM_ configError es

    setSGR [Reset]

configError :: ParseConfigError -> IO ()
configError ParseConfigError{ pcePath = path, pceParseError = msg} = do
    setSGR [SetConsoleIntensity BoldIntensity]
    putStrLn path
    setSGR [Reset]
    putStrLn $ "    " ++ msg
