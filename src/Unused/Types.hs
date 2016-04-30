module Unused.Types
    ( TermMatch(..)
    , TermResults(..)
    , TermMatchSet
    , ParseResponse
    , DirectoryPrefix(..)
    , listFromMatchSet
    , withOneFile
    , withOneOccurrence
    , resultsFromMatches
    , responsesGroupedByPath
    ) where

import System.FilePath (takeDirectory, splitDirectories)
import Text.Parsec (ParseError)
import Data.Bifunctor (second)
import Data.List (intercalate, sort, nub)
import qualified Data.Map.Strict as Map

data TermMatch = TermMatch
    { term :: String
    , path :: String
    , occurrences :: Int
    } deriving Show

data TermResults = TermResults
    { matches :: [TermMatch]
    , totalFiles :: Int
    , totalOccurrences :: Int
    } deriving Show

type TermMatchSet = Map.Map String TermResults

type ParseResponse = Either ParseError TermMatchSet

newtype DirectoryPrefix = DirectoryPrefix String deriving (Eq, Show, Ord)

resultsFromMatches :: [TermMatch] -> TermResults
resultsFromMatches m =
    TermResults
        { matches = m
        , totalFiles = totalFiles'
        , totalOccurrences = totalOccurrences'
        }
  where
    totalFiles' = length m
    totalOccurrences' = sum $ fmap occurrences m

withOneFile :: ParseResponse -> ParseResponse
withOneFile = fmap $ Map.filterWithKey (const $ ((==) 1) . totalFiles)

withOneOccurrence :: ParseResponse -> ParseResponse
withOneOccurrence = fmap $ Map.filterWithKey (const $ ((==) 1 ) . totalOccurrences)

listFromMatchSet :: TermMatchSet -> [(String, TermResults)]
listFromMatchSet =
  Map.toList

responsesGroupedByPath :: TermMatchSet -> [(DirectoryPrefix, TermMatchSet)]
responsesGroupedByPath pr =
    fmap (\p -> (p, responseForPath p pr)) $ directoriesForGrouping pr

responseForPath :: DirectoryPrefix -> TermMatchSet -> TermMatchSet
responseForPath s =
    filterVByPath . filterKVByPath
  where
    filterVByPath = Map.map (resultsFromMatches . filter (((==) s) . fileNameGrouping . path) . matches)
    filterKVByPath = Map.filterWithKey (\_ a -> s `elem` allPaths a)
    allPaths = fmap (fileNameGrouping . path) . matches

fileNameGrouping :: String -> DirectoryPrefix
fileNameGrouping =
    DirectoryPrefix . grouping
  where
    grouping = intercalate "/" . take 2 . splitDirectories . takeDirectory

directoriesForGrouping :: TermMatchSet -> [DirectoryPrefix]
directoriesForGrouping =
    uniqueValues . Map.map (fmap (fileNameGrouping . path) . matches)
  where
    uniqueValues = sort . nub . concat . Map.elems
