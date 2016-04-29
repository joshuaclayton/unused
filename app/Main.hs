module Main where

import System.Console.ANSI
import Unused.TermSearch (search)
import Unused.Parser (parseLines)
import Unused.Types
import Data.Map.Strict (toList)

main :: IO ()
main = do
    terms <- pure . lines =<< getContents
    results <- pure . concat =<< mapM search terms

    case parseLines $ unlines results of
        ValidParse termMatchSet ->
            mapM_ printMatchPair $ toList termMatchSet
        InvalidParse e -> do
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

    return ()

printMatchPair :: (String, [TermMatch]) -> IO ()
printMatchPair (term', matches) = do
    setSGR   [SetColor Foreground Vivid Red]
    setSGR   [SetConsoleIntensity BoldIntensity]
    putStrLn term'
    setSGR   [Reset]
    printMatches matches
    putStr "\n"

printMatches :: [TermMatch] -> IO ()
printMatches matches = do
    mapM_ printMatch matches
  where
    printMatch m = do
        setSGR [SetColor Foreground Dull Green]
        putStr $ "  " ++ path m
        setSGR [SetColor Foreground Dull Yellow]
        putStr $ " " ++ (show . occurrences) m ++ " "
        setSGR [Reset]
        putStr "\n"
