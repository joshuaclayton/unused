module Unused.ResponseFilter
    ( withOneOccurrence
    , withLikelihoods
    , oneOccurence
    , ignoringPaths
    , isClassOrModule
    , autoLowLikelihood
    , updateMatches
    ) where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Unused.ResultsClassifier
       (LanguageConfiguration(..), LowLikelihoodMatch(..), Matcher(..),
        Position(..))
import Unused.Types
       (Removal(..), RemovalLikelihood, TermMatch(..), TermMatchSet,
        TermResults(..), appOccurrenceCount, totalOccurrenceCount)

withOneOccurrence :: TermMatchSet -> TermMatchSet
withOneOccurrence = Map.filterWithKey (const oneOccurence)

oneOccurence :: TermResults -> Bool
oneOccurence = (== 1) . totalOccurrenceCount

withLikelihoods :: [RemovalLikelihood] -> TermMatchSet -> TermMatchSet
withLikelihoods [] = id
withLikelihoods l = Map.filterWithKey (const $ includesLikelihood l)

ignoringPaths :: [String] -> TermMatchSet -> TermMatchSet
ignoringPaths xs = updateMatches newMatches
  where
    newMatches = filter (not . matchesPath . tmPath)
    matchesPath p = any (`L.isInfixOf` p) xs

includesLikelihood :: [RemovalLikelihood] -> TermResults -> Bool
includesLikelihood l = (`elem` l) . rLikelihood . trRemoval

isClassOrModule :: TermResults -> Bool
isClassOrModule = startsWithUpper . trTerm
  where
    startsWithUpper [] = False
    startsWithUpper (a:_) = C.isUpper a

autoLowLikelihood :: LanguageConfiguration -> TermResults -> Bool
autoLowLikelihood l r = isAllowedTerm r allowedTerms || or anySinglesOkay
  where
    allowedTerms = lcAllowedTerms l
    anySinglesOkay = map (\sm -> classOrModule sm r && matchesToBool (smMatchers sm)) singles
    singles = lcAutoLowLikelihood l
    classOrModule = classOrModuleFunction . smClassOrModule
    matchesToBool :: [Matcher] -> Bool
    matchesToBool [] = False
    matchesToBool a = all (`matcherToBool` r) a

classOrModuleFunction :: Bool -> TermResults -> Bool
classOrModuleFunction True = isClassOrModule
classOrModuleFunction False = const True

matcherToBool :: Matcher -> TermResults -> Bool
matcherToBool (Path p v) = any (positionToTest p v) . paths
matcherToBool (Term p v) = positionToTest p v . trTerm
matcherToBool (AppOccurrences i) = (== i) . appOccurrenceCount
matcherToBool (AllowedTerms ts) = (`isAllowedTerm` ts)

positionToTest :: Position -> (String -> String -> Bool)
positionToTest StartsWith = L.isPrefixOf
positionToTest EndsWith = L.isSuffixOf
positionToTest Equals = (==)

paths :: TermResults -> [String]
paths = fmap tmPath . trMatches

updateMatches :: ([TermMatch] -> [TermMatch]) -> TermMatchSet -> TermMatchSet
updateMatches fm = Map.map (updateMatchesWith $ fm . trMatches)
  where
    updateMatchesWith f tr = tr {trMatches = f tr}

isAllowedTerm :: TermResults -> [String] -> Bool
isAllowedTerm = elem . trTerm
