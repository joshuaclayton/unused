module Unused.Cache.FindArgsFromIgnoredPaths
    ( findArgs
    ) where

import Data.Char (isAlphaNum)
import Data.List (isSuffixOf)
import System.FilePath

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
    | "/" `isSuffixOf` s = s ++ "*"
    | otherwise = s ++ "/*"

isWildcardFilename :: String -> Bool
isWildcardFilename = elem '*' . takeFileName

isMissingFilename :: String -> Bool
isMissingFilename s = takeFileName s == ""

validIgnoreOptions :: [String] -> [String]
validIgnoreOptions =
    filter isPath
  where
    isPath "" = False
    isPath ('/':_) = True
    isPath ('.':_) = True
    isPath s = isAlphaNum $ head s
