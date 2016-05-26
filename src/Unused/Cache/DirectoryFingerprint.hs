module Unused.Cache.DirectoryFingerprint
    ( sha
    ) where

import System.Process
import Data.Maybe (fromMaybe)
import Unused.Cache.FindArgsFromIgnoredPaths
import Unused.Util (readIfFileExists)

sha :: IO String
sha =
    getSha <$> (fileList >>= sortInput >>= md5Result)
  where
    getSha = head' . lines
    head' (x:_) = x
    head' _ = ""

fileList :: IO String
fileList = do
    filterNamePathArgs <- findArgs <$> ignoredPaths
    let args = [".", "-type", "f", "-not", "-path", "*/.git/*"] ++ filterNamePathArgs ++ ["-exec", "md5", "{}", "+"]
    readProcess "find" args ""

sortInput :: String -> IO String
sortInput = readProcess "sort" ["-k", "2"]

md5Result :: String -> IO String
md5Result = readProcess "md5" []

ignoredPaths :: IO [String]
ignoredPaths = fromMaybe [] <$> (fmap lines <$> readIfFileExists ".gitignore")
