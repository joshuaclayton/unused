module Unused.CLI.Views.SearchResult.ListResult
    ( printList
    ) where

import qualified Control.Monad as M
import           Data.List ((\\))
import qualified Data.List as L
import           Unused.CLI.Util
import qualified Unused.CLI.Views.SearchResult.Internal as SR
import qualified Unused.CLI.Views.SearchResult.Types as SR
import           Unused.Types (TermResults(..), GitContext(..), GitCommit(..), TermMatch(..), tmDisplayTerm, totalFileCount, totalOccurrenceCount)

printList :: TermResults -> [TermMatch] -> SR.ResultsPrinter ()
printList r ms = SR.liftIO $
    M.forM_ ms $ \m -> do
        printTermAndOccurrences r m
        printAliases r
        printFilePath m
        printSHAs r
        printRemovalReason r
        putStr "\n"

printTermAndOccurrences :: TermResults -> TermMatch -> IO ()
printTermAndOccurrences r m = do
    setSGR [SetColor Foreground Dull (SR.termColor r)]
    setSGR [SetConsoleIntensity BoldIntensity]
    putStr "  "
    setSGR [SetUnderlining SingleUnderline]
    putStr $ tmDisplayTerm m
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
printAliases r = M.when anyAliases $ do
    printHeader "    Aliases: "
    putStrLn $ L.intercalate ", " remainingAliases
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
        Nothing -> M.void $ putStr ""
        Just shas' -> do
            printHeader "    Recent SHAs: "
            putStrLn $ L.intercalate ", " shas'
  where
    mshas = (map gcSha . gcCommits) <$> trGitContext r

printRemovalReason :: TermResults -> IO ()
printRemovalReason r = do
    printHeader "    Reason: "
    putStrLn $ SR.removalReason r

printHeader :: String -> IO ()
printHeader v = do
    setSGR [SetConsoleIntensity BoldIntensity]
    putStr v
    setSGR [SetConsoleIntensity NormalIntensity]

pluralize :: Int -> String -> String -> String
pluralize i@1 singular _ = show i ++ " " ++ singular
pluralize i _ plural = show i ++ " " ++ plural
