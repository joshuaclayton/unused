module Unused.Cache.DirectoryFingerprint
    ( sha
    ) where

import System.Process

sha :: IO String
sha =
    getSha <$> (fileList >>= sortInput >>= md5Result)
  where
    getSha = head' . lines
    head' (x:_) = x
    head' _ = ""

fileList :: IO String
fileList = readProcess "find" [".", "-type", "f", "-not", "-path", "*/tmp/unused/*", "-exec", "md5", "{}", "+"] ""

sortInput :: String -> IO String
sortInput = readProcess "sort" ["-k", "2"]

md5Result :: String -> IO String
md5Result = readProcess "md5" []
