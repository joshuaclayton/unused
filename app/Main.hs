module Main where

import Unused.TermSearch (search)

main :: IO ()
main = do
    tokens <- pure . lines =<< getContents
    results <- pure . concat =<< mapM search tokens
    putStrLn $ unlines results
