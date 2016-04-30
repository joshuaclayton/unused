module Main where

import Control.Monad.State
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import System.Console.ANSI
import System.ProgressBar
import Unused.TermSearch (search)
import Unused.Parser (parseLines, ParseError)
import Unused.Types

main :: IO ()
main = do
    terms <- pure . lines =<< getContents
    results <- executeSearch terms
    let response = parseLines $ unlines results

    case withOneOccurrence $ withOneFile response of
        Right termMatchSet -> do
            clearScreen
            let responses = responsesGroupedByPath termMatchSet
            mapM_ printDirectorySection responses
        Left e ->
            printParseError e

    return ()

executeSearch :: [String] -> IO [String]
executeSearch terms = do
    resetScreen
    printAnalysisHeader terms
    (results, _) <- runStateT (performSearch $ length terms) terms
    resetScreen

    return $ concat results

performSearch :: Int -> StateT [String] IO [[String]]
performSearch total = do
    currentTerm <- getSearchTerm
    searchResults <- liftIO $ search currentTerm

    remainingTerms <- get
    let remaining = length remainingTerms

    liftIO $ printProgressBar (total - remaining) total

    if remaining > 0
        then do
            res <- performSearch total
            return $ searchResults:res
        else return [searchResults]

getSearchTerm :: StateT [String] IO String
getSearchTerm = do
    (x:xs) <- get
    put xs
    return x

printProgressBar :: Int -> Int -> IO ()
printProgressBar complete total = do
    let message = "Working"
    let progressBarWidth = 60

    hSetBuffering stdout NoBuffering
    progressBar (msg message) percentage progressBarWidth (toInteger complete) (toInteger total)

printAnalysisHeader :: [String] -> IO ()
printAnalysisHeader terms = do
    setSGR [SetConsoleIntensity BoldIntensity]
    putStr "Unused: "
    setSGR [Reset]

    putStr "analyzing "

    setSGR [SetColor Foreground Dull Green]
    putStr $ show $ length terms
    setSGR [Reset]
    putStr " terms\n\n"

printDirectorySection :: (DirectoryPrefix, TermMatchSet) -> IO ()
printDirectorySection (dir, ss) = do
    printDirectory dir
    mapM_ printTermResults $ listFromMatchSet ss
    putStr "\n"

printDirectory :: DirectoryPrefix -> IO ()
printDirectory (DirectoryPrefix dir) = do
    setSGR   [SetColor Foreground Vivid Black]
    setSGR   [SetConsoleIntensity BoldIntensity]
    putStrLn dir
    setSGR   [Reset]

printTermResults :: (String, TermResults) -> IO ()
printTermResults (_, results) = do
    printMatches results $ matches results

printMatches :: TermResults -> [TermMatch] -> IO ()
printMatches _r ms = do
    mapM_ printMatch ms
  where
    printMatch m = do
        setSGR [SetColor Foreground Dull Red]
        setSGR [SetConsoleIntensity NormalIntensity]
        putStr $ "     " ++ term m
        setSGR [Reset]
        setSGR [SetColor Foreground Dull Cyan]
        setSGR [SetConsoleIntensity FaintIntensity]
        putStr $ "  " ++ path m
        setSGR [Reset]
        putStr "\n"

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

resetScreen :: IO ()
resetScreen = do
    clearScreen
    setCursorPosition 0 0
