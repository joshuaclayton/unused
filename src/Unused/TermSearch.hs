module Unused.TermSearch
    ( search
    ) where

import System.Process

search :: String -> IO [String]
search t = do
    results <- ag t
    return $ linesMap prefixTerm results
  where
    prefixTerm = ((++) t)

linesMap :: (String -> String) -> String -> [String]
linesMap f =
    filter empty . map f . lines
  where
    empty = (/= 0) . length

ag :: String -> IO String
ag t = do
  (_, results, _) <- readProcessWithExitCode "ag" [t, ".", "-c", "-Q", "--ackmate"] ""
  return results
