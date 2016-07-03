module Unused.Cache.FindArgsFromIgnoredPaths
    ( findArgs
    ) where

import qualified Data.Char as C
import qualified Data.List as L
import qualified System.FilePath as FP

findArgs :: [String] -> [String]
findArgs = concatMap ignoreToFindArgs . validIgnoreOptions

wildcardPrefix :: String -> String
wildcardPrefix a@('*':'/':_) = a
wildcardPrefix ('*':s) = "*/" ++ s
wildcardPrefix ('/':s) = "*/" ++ s
wildcardPrefix a = "*/" ++ a

toExclusions :: String -> [String]
toExclusions s =
    case (isWildcardFilename s, isMissingFilename s) of
        (True, _) -> ["-not", "-path", s]
        (_, True) -> ["-not", "-path", wildcardSuffix s]
        (_, False) -> ["-not", "-name", s, "-not", "-path", wildcardSuffix s]

ignoreToFindArgs :: String -> [String]
ignoreToFindArgs = toExclusions . wildcardPrefix

wildcardSuffix :: String -> String
wildcardSuffix s
    | isWildcardFilename s = s
    | "/" `L.isSuffixOf` s = s ++ "*"
    | otherwise = s ++ "/*"

isWildcardFilename :: String -> Bool
isWildcardFilename = elem '*' . FP.takeFileName

isMissingFilename :: String -> Bool
isMissingFilename = null . FP.takeFileName

validIgnoreOptions :: [String] -> [String]
validIgnoreOptions =
    filter isPath
  where
    isPath "" = False
    isPath ('/':_) = True
    isPath ('.':_) = True
    isPath s = C.isAlphaNum $ head s
