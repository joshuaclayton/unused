module Unused.CLI.Views.SearchResult.ListResult
    ( printList
    ) where

import Control.Monad (forM_, void, when)
import Data.List (intercalate, (\\))
import Unused.CLI.Util
import Unused.Types
import Unused.CLI.Views.SearchResult.Internal
import Unused.CLI.Views.SearchResult.Types

printList :: TermResults -> [TermMatch] -> ResultsPrinter ()
printList r ms = liftIO $
    forM_ ms $ \m -> do
        printTermAndOccurrences r
        printAliases r
        printFilePath m
        printSHAs r
        printRemovalReason r
        putStr "\n"

printTermAndOccurrences :: TermResults -> IO ()
printTermAndOccurrences r = do
    setSGR [SetColor Foreground Dull (termColor r)]
    setSGR [SetConsoleIntensity BoldIntensity]
    putStr "  "
    setSGR [SetUnderlining SingleUnderline]
    putStr $ trTerm r
    setSGR [Reset]

    setSGR [SetColor Foreground Vivid Cyan]
    setSGR [SetConsoleIntensity NormalIntensity]
    putStr " ("
    putStr $ pluralize (totalFileCount r) "file" "files"
    putStr ", "
    putStr $ pluralize (totalOccurrenceCount r) "occurrence" "occurrences"
    putStr ")"
    setSGR [Reset]
    putStr "\n"

printAliases :: TermResults -> IO ()
printAliases r = when anyAliases $ do
    printHeader "    Aliases: "
    putStrLn $ intercalate ", " remainingAliases
  where
    anyAliases = not $ null remainingAliases
    remainingAliases = trTerms r \\ [trTerm r]

printFilePath :: TermMatch -> IO ()
printFilePath m = do
    printHeader "    File Path: "
    setSGR [SetColor Foreground Dull Cyan]
    putStrLn $ tmPath m
    setSGR [Reset]

printSHAs :: TermResults -> IO ()
printSHAs r =
    case mshas of
        Nothing -> void $ putStr ""
        Just shas' -> do
            printHeader "    Recent SHAs: "
            putStrLn $ intercalate ", " shas'
  where
    mshas = (map gcSha . gcCommits) <$> trGitContext r

printRemovalReason :: TermResults -> IO ()
printRemovalReason r = do
    printHeader "    Reason: "
    putStrLn $ removalReason r

printHeader :: String -> IO ()
printHeader v = do
    setSGR [SetConsoleIntensity BoldIntensity]
    putStr v
    setSGR [SetConsoleIntensity NormalIntensity]

pluralize :: Int -> String -> String -> String
pluralize i@1 singular _ = show i ++ " " ++ singular
pluralize i _ plural = show i ++ " " ++ plural
