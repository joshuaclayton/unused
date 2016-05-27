module Unused.CLI.Views.MissingTagsFileError
    ( missingTagsFileError
    ) where

import Unused.TagsSource
import Unused.CLI.Util

missingTagsFileError :: TagSearchOutcome -> IO ()
missingTagsFileError e = do
    setSGR [SetColor Background Vivid Red]
    setSGR [SetColor Foreground Vivid White]
    setSGR [SetConsoleIntensity BoldIntensity]

    putStrLn "\nThere was a problem finding a tags file.\n"

    setSGR [Reset]

    printOutcomeMessage e
    putStr "\n"

    setSGR [SetConsoleIntensity BoldIntensity]
    putStr "If you're generating a ctags file to a custom location, "
    putStrLn "you can pipe it into unused:"
    setSGR [Reset]

    putStrLn "    cat custom/ctags | unused --stdin"

    putStr "\n"

    setSGR [SetConsoleIntensity BoldIntensity]
    putStrLn "You can find out more about Exuberant Ctags here:"
    setSGR [Reset]
    putStrLn "    http://ctags.sourceforge.net/"

    putStr "\n"

    setSGR [SetConsoleIntensity BoldIntensity]
    putStrLn "You can read about a good git-based Ctags workflow here:"
    setSGR [Reset]
    putStrLn "    http://tbaggery.com/2011/08/08/effortless-ctags-with-git.html"

    putStr "\n"

printOutcomeMessage :: TagSearchOutcome -> IO ()
printOutcomeMessage (TagsFileNotFound directoriesSearched) = do
    putStrLn "Looked for a 'tags' file in the following directories:\n"
    mapM_ (\d -> putStrLn $ "* " ++ d) directoriesSearched
