module Unused.Types
    ( TermMatch(..)
    , TermResults(..)
    , TermMatchSet
    , ParseResponse
    , DirectoryPrefix(..)
    , RemovalLikelihood(..)
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
import Unused.Regex (matchRegex)

data TermMatch = TermMatch
    { term :: String
    , path :: String
    , occurrences :: Int
    } deriving Show

data TermResults = TermResults
    { resultTerm :: String
    , matches :: [TermMatch]
    , totalFiles :: Int
    , totalOccurrences :: Int
    , removalLikelihood :: RemovalLikelihood
    } deriving Show

data RemovalLikelihood = High | Medium | Low deriving Show

type TermMatchSet = Map.Map String TermResults

type ParseResponse = Either ParseError TermMatchSet

newtype DirectoryPrefix = DirectoryPrefix String deriving (Eq, Show, Ord)

resultsFromMatches :: [TermMatch] -> TermResults
resultsFromMatches m =
    calculateLikelihood $ TermResults
        { resultTerm = resultTerm' terms
        , matches = m
        , totalFiles = totalFiles'
        , totalOccurrences = totalOccurrences'
        , removalLikelihood = High
        }
  where
    totalFiles' = length m
    totalOccurrences' = sum $ fmap occurrences m
    terms = map term m
    resultTerm' (x:_) = x
    resultTerm' _ = ""

withOneFile :: ParseResponse -> ParseResponse
withOneFile = fmap $ Map.filterWithKey (const oneFile)

withOneOccurrence :: ParseResponse -> ParseResponse
withOneOccurrence = fmap $ Map.filterWithKey (const oneOccurence)

oneOccurence :: TermResults -> Bool
oneOccurence = (== 1) . totalOccurrences

oneFile :: TermResults -> Bool
oneFile = (== 1) . totalFiles

isClassOrModule :: TermResults -> Bool
isClassOrModule = matchRegex "^[A-Z]" . resultTerm

railsSingleOkay :: TermResults -> Bool
railsSingleOkay r =
    foldl1 (&&) [isClassOrModule r, oneFile r, oneOccurence r, (controller || helper || migration)]
  where
    controller = (matchRegex "^app/controllers/" singlePath) && (matchRegex "Controller$" $ resultTerm r)
    helper = (matchRegex "^app/helpers/" singlePath) && (matchRegex "Helper$" $ resultTerm r)
    migration = matchRegex "^db/migrate/" singlePath
    singlePath = path' $ fmap path $ matches r
    path' (x:_) = x
    path' [] = ""

elixirSingleOkay :: TermResults -> Bool
elixirSingleOkay r =
    foldl1 (&&) [isClassOrModule r, oneFile r, oneOccurence r, (view || test || migration)]
  where
    migration = matchRegex "^priv/repo/migrations/" singlePath
    view = (matchRegex "^web/views/" singlePath) && (matchRegex "View$" $ resultTerm r)
    test = (matchRegex "^test/" singlePath) && (matchRegex "Test$" $ resultTerm r)
    singlePath = path' $ fmap path $ matches r
    path' (x:_) = x
    path' [] = ""

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

calculateLikelihood :: TermResults -> TermResults
calculateLikelihood r =
    r { removalLikelihood = newLikelihood }
  where
    baseScore = totalOccurrences r
    railsScore = if railsSingleOkay r then 5 else 0
    elixirScore = if elixirSingleOkay r then 5 else 0
    totalScore = baseScore + railsScore + elixirScore
    newLikelihood
        | totalScore < 3 = High
        | totalScore < 6 = Medium
        | totalScore < 9 = Low
        | otherwise = Low
