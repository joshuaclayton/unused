module Main where

import Unused.TermSearch (search)
import Unused.Parser (parseLines)

main :: IO ()
main = do
    terms <- pure . lines =<< getContents
    results <- pure . concat =<< mapM search terms
    let matches = parseLines $ unlines results

    print matches
