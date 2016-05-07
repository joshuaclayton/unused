module Unused.CLI.SearchResult
    ( printSearchResults
    ) where

import Control.Monad (forM_)
import Text.Printf
import qualified Data.Map.Strict as Map
import Unused.Types
import Unused.DirectoryGrouping (DirectoryPrefix(..), responsesGroupedByPath)
import Unused.CLI.Util

printSearchResults :: TermMatchSet -> IO ()
printSearchResults termMatchSet =
    mapM_ (printDirectorySection maxWidth) responses
  where
    responses = responsesGroupedByPath termMatchSet
    allSets = listFromMatchSet =<< map snd responses
    allResults = map snd allSets
    termLength = return . length . tmTerm
    maxWidth = maximum $ termLength =<< trMatches =<< allResults

listFromMatchSet :: TermMatchSet -> [(String, TermResults)]
listFromMatchSet =
  Map.toList

printDirectorySection :: Int -> (DirectoryPrefix, TermMatchSet) -> IO ()
printDirectorySection w (dir, ss) = do
    printDirectory dir
    mapM_ (printTermResults w) $ listFromMatchSet ss
    putStr "\n"

printDirectory :: DirectoryPrefix -> IO ()
printDirectory (DirectoryPrefix dir) = do
    setSGR   [SetColor Foreground Vivid Black]
    setSGR   [SetConsoleIntensity BoldIntensity]
    putStrLn dir
    setSGR   [Reset]

printTermResults :: Int -> (String, TermResults) -> IO ()
printTermResults w (_, results) =
    printMatches w results $ trMatches results

likelihoodColor :: RemovalLikelihood -> Color
likelihoodColor High = Red
likelihoodColor Medium = Yellow
likelihoodColor Low = Green

printMatches :: Int -> TermResults -> [TermMatch] -> IO ()
printMatches w r ms =
    forM_ ms $ \m -> do
        setSGR [SetColor Foreground Dull (likelihoodColor $ trRemovalLikelihood r)]
        setSGR [SetConsoleIntensity NormalIntensity]
        putStr $ "     " ++ printf termFormat (tmTerm m)
        setSGR [Reset]

        setSGR [SetColor Foreground Vivid Cyan]
        setSGR [SetConsoleIntensity NormalIntensity]
        putStr $ printNumber (trTotalFiles r) ++ "," ++ printNumber (trTotalOccurrences r) ++ " "
        setSGR [Reset]

        setSGR [SetColor Foreground Dull Cyan]
        setSGR [SetConsoleIntensity FaintIntensity]
        putStr $ "  " ++ tmPath m
        setSGR [Reset]
        putStr "\n"
  where
    termFormat = "%-" ++ show w ++ "s"
    printNumber = printf "%2d"
