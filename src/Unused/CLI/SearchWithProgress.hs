module Unused.CLI.SearchWithProgress
    ( searchWithProgressBar
    ) where

import Control.Monad.State
import System.ProgressBar
import Unused.TermSearch (search)

searchWithProgressBar :: [String] -> IO [String]
searchWithProgressBar terms =
    (concat . fst) <$> runStateT (performSearch $ length terms) terms

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

    progressBar (msg message) percentage progressBarWidth (toInteger complete) (toInteger total)
