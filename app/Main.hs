module Main where

import System.Console.ANSI
import Data.List (nub)
import Unused.TermSearch (search)
import Unused.Parser (parseLines)
import Unused.Types

main :: IO ()
main = do
    terms <- pure . lines =<< getContents
    results <- pure . concat =<< mapM search terms
    let groupedMatches = groupBy term $ parseLines $ unlines results

    mapM_ printMatchPair groupedMatches

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

groupBy :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupBy f l =
    fmap (\t -> (t, byTerm t)) uniqueTerms
  where
    byTerm t = filter (((==) t) . f) l
    uniqueTerms = nub $ fmap f l
