module Unused.ResponseFilter
    ( withOneOccurrence
    , withLikelihoods
    , oneOccurence
    , ignoringPaths
    , isClassOrModule
    , autoLowLikelihood
    , updateMatches
    ) where

import qualified Data.Map.Strict as Map
import Data.List (isInfixOf)
import Unused.Regex (matchRegex)
import Unused.Types
import Unused.ResultsClassifier

withOneOccurrence :: TermMatchSet -> TermMatchSet
withOneOccurrence = Map.filterWithKey (const oneOccurence)

oneOccurence :: TermResults -> Bool
oneOccurence = (== 1) . totalOccurrenceCount

withLikelihoods :: [RemovalLikelihood] -> TermMatchSet -> TermMatchSet
withLikelihoods [] = id
withLikelihoods l = Map.filterWithKey (const $ includesLikelihood l)

ignoringPaths :: [String] -> TermMatchSet -> TermMatchSet
ignoringPaths xs =
    updateMatches newMatches
  where
    newMatches = filter (not . matchesPath . tmPath)
    matchesPath p = any (`isInfixOf` p) xs

includesLikelihood :: [RemovalLikelihood] -> TermResults -> Bool
includesLikelihood l = (`elem` l) . rLikelihood . trRemoval

isClassOrModule :: TermResults -> Bool
isClassOrModule = matchRegex "^[A-Z]" . trTerm

autoLowLikelihood :: LanguageConfiguration -> TermResults -> Bool
autoLowLikelihood l r =
    isAllowedTerm r allowedTerms || or anySinglesOkay
  where
    allowedTerms = lcAllowedTerms l
    anySinglesOkay = map (\sm -> classOrModule sm r && matchesToBool (smMatchers sm)) singles
    singles = lcAutoLowLikelihood l
    classOrModule = classOrModuleFunction . smClassOrModule

    matchesToBool :: [Matcher] -> Bool
    matchesToBool = all (`matcherToBool` r)

classOrModuleFunction :: Bool -> TermResults -> Bool
classOrModuleFunction True = isClassOrModule
classOrModuleFunction False = const True

matcherToBool :: Matcher -> TermResults -> Bool
matcherToBool (Path p v) = any (positionToRegex p v) . paths
matcherToBool (Term p v) = positionToRegex p v . trTerm
matcherToBool (AppOccurrences i) = (== i) . appOccurrenceCount
matcherToBool (AllowedTerms ts) = flip isAllowedTerm ts

positionToRegex :: Position -> (String -> String -> Bool)
positionToRegex StartsWith = \v -> matchRegex ("^" ++ v)
positionToRegex EndsWith = \v -> matchRegex (v ++ "$")
positionToRegex Equals = (==)

paths :: TermResults -> [String]
paths r = tmPath <$> trMatches r

updateMatches :: ([TermMatch] -> [TermMatch]) -> TermMatchSet -> TermMatchSet
updateMatches fm =
    Map.map (updateMatchesWith $ fm . trMatches)
  where
    updateMatchesWith f tr = tr { trMatches = f tr }

isAllowedTerm :: TermResults -> [String] -> Bool
isAllowedTerm = elem . trTerm
