{-# LANGUAGE DeriveGeneric #-}

module Unused.Types
    ( SearchTerm(..)
    , TermMatch(..)
    , TermResults(..)
    , TermMatchSet
    , RemovalLikelihood(..)
    , Removal(..)
    , Occurrences(..)
    , GitContext(..)
    , GitCommit(..)
    , searchTermToString
    , resultsFromMatches
    , tmDisplayTerm
    , totalFileCount
    , totalOccurrenceCount
    , appOccurrenceCount
    , removalLikelihood
    , resultAliases
    ) where

import           Control.Monad (liftM2)
import           Data.Csv (FromRecord, ToRecord)
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Map.Strict as Map
import qualified GHC.Generics as G
import qualified Unused.Regex as R

data SearchTerm
    = OriginalTerm String
    | AliasTerm String String deriving (Eq, Show)

searchTermToString :: SearchTerm -> String
searchTermToString (OriginalTerm s) = s
searchTermToString (AliasTerm _ a) = a

data TermMatch = TermMatch
    { tmTerm :: String
    , tmPath :: String
    , tmAlias :: Maybe String
    , tmOccurrences :: Int
    } deriving (Eq, Show, G.Generic)

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

tmDisplayTerm :: TermMatch -> String
tmDisplayTerm = liftM2 M.fromMaybe tmTerm tmAlias

resultsFromMatches :: [TermMatch] -> TermResults
resultsFromMatches tms =
    TermResults
        { trTerm = resultTerm terms
        , trTerms = L.sort $ L.nub terms
        , trMatches = tms
        , trAppOccurrences = appOccurrence
        , trTestOccurrences = testOccurrence
        , trTotalOccurrences = Occurrences (sum $ map oFiles [appOccurrence, testOccurrence]) (sum $ map oOccurrences [appOccurrence, testOccurrence])
        , trRemoval = Removal NotCalculated "Likelihood not calculated"
        , trGitContext = Nothing
        }
  where
    testOccurrence = testOccurrences tms
    appOccurrence = appOccurrences tms
    terms = map tmDisplayTerm tms
    resultTerm (x:_) = x
    resultTerm _ = ""

appOccurrences :: [TermMatch] -> Occurrences
appOccurrences ms =
    Occurrences appFiles appOccurrences'
  where
    totalFiles = length $ L.nub $ map tmPath ms
    totalOccurrences = sum $ map tmOccurrences ms
    tests = testOccurrences ms
    appFiles = totalFiles - oFiles tests
    appOccurrences' = totalOccurrences - oOccurrences tests

testOccurrences :: [TermMatch] -> Occurrences
testOccurrences ms =
    Occurrences totalFiles totalOccurrences
  where
    testMatches = filter termMatchIsTest ms
    totalFiles = length $ L.nub $ map tmPath testMatches
    totalOccurrences = sum $ map tmOccurrences testMatches

testDir :: String -> Bool
testDir = R.matchRegex "(spec|tests?|features)\\/"

testSnakeCaseFilename :: String -> Bool
testSnakeCaseFilename = R.matchRegex ".*(_spec|_test)\\."

testCamelCaseFilename :: String -> Bool
testCamelCaseFilename = R.matchRegex ".*(Spec|Test)\\."

termMatchIsTest :: TermMatch -> Bool
termMatchIsTest TermMatch{tmPath = path} =
    testDir path || testSnakeCaseFilename path || testCamelCaseFilename path
