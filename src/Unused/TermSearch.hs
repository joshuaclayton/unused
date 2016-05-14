module Unused.TermSearch
    ( search
    ) where

import System.Process
import Unused.TermSearch.Internal (commandLineOptions)

search :: String -> IO [String]
search t = do
    results <- ag t
    return $ linesMap suffixTerm results
  where
    suffixTerm = (++ (":" ++ t))

linesMap :: (String -> String) -> String -> [String]
linesMap f =
    filter empty . map f . lines
  where
    empty = not . null

ag :: String -> IO String
ag t = do
  (_, results, _) <- readProcessWithExitCode "ag" (commandLineOptions t) ""
  return results
