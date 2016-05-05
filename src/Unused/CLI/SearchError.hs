module Unused.CLI.SearchError
    ( printParseError
    ) where

import Unused.Parser (ParseError)
import Unused.CLI.Util

printParseError :: ParseError -> IO ()
printParseError e = do
    setSGR [SetColor Background Vivid Red]
    setSGR [SetColor Foreground Vivid White]
    setSGR [SetConsoleIntensity BoldIntensity]

    putStrLn "\nThere was a problem parsing the data:\n"

    setSGR [Reset]

    setSGR [SetColor Foreground Vivid Red]
    setSGR [SetConsoleIntensity BoldIntensity]

    print e
    putStr "\n"

    setSGR [Reset]
