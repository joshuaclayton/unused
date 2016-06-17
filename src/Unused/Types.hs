{-# LANGUAGE DeriveGeneric #-}

module Unused.Types
    ( TermMatch(..)
    , TermResults(..)
    , TermMatchSet
    , RemovalLikelihood(..)
    , Removal(..)
    , Occurrences(..)
    , GitContext(..)
    , GitCommit(..)
    , resultsFromMatches
    , totalFileCount
    , totalOccurrenceCount
    , appOccurrenceCount
    , removalLikelihood
    , resultAliases
    ) where

import qualified Data.Map.Strict as Map
import Data.Csv
import qualified Data.List as L
import GHC.Generics
import Unused.Regex

data TermMatch = TermMatch
    { tmTerm :: String
    , tmPath :: String
    , tmOccurrences :: Int
    } deriving (Eq, Show, Generic)

instance FromRecord TermMatch
instance ToRecord TermMatch

data Occurrences = Occurrences
    { oFiles :: Int
    , oOccurrences :: Int
    } deriving (Eq, Show)

data TermResults = TermResults
    { trTerm :: String
    , trTerms :: [String]
    , trMatches :: [TermMatch]
    , trTestOccurrences :: Occurrences
    , trAppOccurrences :: Occurrences
    , trTotalOccurrences :: Occurrences
    , trRemoval :: Removal
    , trGitContext :: Maybe GitContext
    } deriving (Eq, Show)

data Removal = Removal
    { rLikelihood :: RemovalLikelihood
    , rReason :: String
    } deriving (Eq, Show)

data GitContext = GitContext
    { gcCommits :: [GitCommit]
    } deriving (Eq, Show)

data GitCommit = GitCommit
    { gcSha :: String
    } deriving (Eq, Show)

data RemovalLikelihood = High | Medium | Low | Unknown | NotCalculated deriving (Eq, Show)

type TermMatchSet = Map.Map String TermResults

totalFileCount :: TermResults -> Int
totalFileCount = oFiles . trTotalOccurrences

totalOccurrenceCount :: TermResults -> Int
totalOccurrenceCount = oOccurrences . trTotalOccurrences

appOccurrenceCount :: TermResults -> Int
appOccurrenceCount = oOccurrences . trAppOccurrences

removalLikelihood :: TermResults -> RemovalLikelihood
removalLikelihood = rLikelihood . trRemoval

resultAliases :: TermResults -> [String]
resultAliases = trTerms

resultsFromMatches :: [TermMatch] -> TermResults
resultsFromMatches m =
    TermResults
        { trTerm = resultTerm terms
        , trTerms = L.sort $ L.nub terms
        , trMatches = m
        , trAppOccurrences = appOccurrence
        , trTestOccurrences = testOccurrence
        , trTotalOccurrences = Occurrences (sum $ map oFiles [appOccurrence, testOccurrence]) (sum $ map oOccurrences [appOccurrence, testOccurrence])
        , trRemoval = Removal NotCalculated "Likelihood not calculated"
        , trGitContext = Nothing
        }
  where
    testOccurrence = testOccurrences m
    appOccurrence = appOccurrences m
    terms = map tmTerm m
    resultTerm (x:_) = x
    resultTerm _ = ""

appOccurrences :: [TermMatch] -> Occurrences
appOccurrences ms =
    Occurrences appFiles appOccurrences'
  where
    totalFiles = length ms
    totalOccurrences = sum $ map tmOccurrences ms
    tests = testOccurrences ms
    appFiles = totalFiles - oFiles tests
    appOccurrences' = totalOccurrences - oOccurrences tests

testOccurrences :: [TermMatch] -> Occurrences
testOccurrences ms =
    Occurrences totalFiles totalOccurrences
  where
    testMatches = filter termMatchIsTest ms
    totalFiles = length testMatches
    totalOccurrences = sum $ map tmOccurrences testMatches

testDir :: String -> Bool
testDir = matchRegex "(spec|tests?|features)\\/"

testSnakeCaseFilename :: String -> Bool
testSnakeCaseFilename = matchRegex ".*(_spec|_test)\\."

testCamelCaseFilename :: String -> Bool
testCamelCaseFilename = matchRegex ".*(Spec|Test)\\."

termMatchIsTest :: TermMatch -> Bool
termMatchIsTest m =
    testDir path || testSnakeCaseFilename path || testCamelCaseFilename path
  where
    path = tmPath m
