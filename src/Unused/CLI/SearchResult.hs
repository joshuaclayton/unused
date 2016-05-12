module Unused.CLI.SearchResult
    ( printSearchResults
    ) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Unused.Types
import Unused.DirectoryGrouping (DirectoryPrefix(..), responsesGroupedByPath)
import Unused.CLI.SearchResult.ColumnFormatter
import Unused.CLI.Util

printSearchResults :: TermMatchSet -> IO ()
printSearchResults termMatchSet =
    printFormattedResponses columnFormat responses
  where
    responses = responsesGroupedByPath termMatchSet
    allSets = listFromMatchSet =<< map snd responses
    allResults = map snd allSets
    columnFormat = buildColumnFormatter allResults

printFormattedResponses :: ColumnFormat -> [(DirectoryPrefix, TermMatchSet)] -> IO ()
printFormattedResponses _ [] = printNoResultsFound
printFormattedResponses cf r = mapM_ (printDirectorySection cf) r

listFromMatchSet :: TermMatchSet -> [(String, TermResults)]
listFromMatchSet =
  Map.toList

printDirectorySection :: ColumnFormat -> (DirectoryPrefix, TermMatchSet) -> IO ()
printDirectorySection cf (dir, ss) = do
    printDirectory dir
    mapM_ (printTermResults cf) $ listFromMatchSet ss
    putStr "\n"

printDirectory :: DirectoryPrefix -> IO ()
printDirectory (DirectoryPrefix dir) = do
    setSGR   [SetColor Foreground Vivid Black]
    setSGR   [SetConsoleIntensity BoldIntensity]
    putStrLn dir
    setSGR   [Reset]

printTermResults :: ColumnFormat -> (String, TermResults) -> IO ()
printTermResults cf (_, results) =
    printMatches cf results $ trMatches results

likelihoodColor :: RemovalLikelihood -> Color
likelihoodColor High = Red
likelihoodColor Medium = Yellow
likelihoodColor Low = Green
likelihoodColor Unknown = Black
likelihoodColor NotCalculated = Magenta

printMatches :: ColumnFormat -> TermResults -> [TermMatch] -> IO ()
printMatches cf r ms =
    forM_ ms $ \m -> do
        setSGR [SetColor Foreground Dull (termColor r)]
        setSGR [SetConsoleIntensity NormalIntensity]
        putStr $ "     " ++ printTerm (tmTerm m)
        setSGR [Reset]

        setSGR [SetColor Foreground Vivid Cyan]
        setSGR [SetConsoleIntensity NormalIntensity]
        putStr $ "  " ++ printNumber (totalFileCount r) ++ ", " ++ printNumber (totalOccurrenceCount r)
        setSGR [Reset]

        setSGR [SetColor Foreground Dull Cyan]
        setSGR [SetConsoleIntensity FaintIntensity]
        putStr $ "  " ++ printPath (tmPath m)
        setSGR [Reset]

        putStr $ "  " ++ removalReason r
        putStr "\n"
  where
    printTerm = cfPrintTerm cf
    printPath = cfPrintPath cf
    printNumber = cfPrintNumber cf
    termColor = likelihoodColor . rLikelihood . trRemoval
    removalReason = rReason . trRemoval

printNoResultsFound :: IO ()
printNoResultsFound = do
    setSGR   [SetColor Foreground Dull Green]
    setSGR   [SetConsoleIntensity BoldIntensity]
    putStrLn "Unused found no results"
    setSGR   [Reset]
