module Unused.ResponseFilter
    ( withOneOccurrence
    , withLikelihoods
    , oneOccurence
    , ignoringPaths
    , isClassOrModule
    , railsSingleOkay
    , elixirSingleOkay
    , updateMatches
    ) where

import qualified Data.Map.Strict as Map
import Data.List (isInfixOf)
import Unused.Regex (matchRegex)
import Unused.Types

withOneOccurrence :: ParseResponse -> ParseResponse
withOneOccurrence = applyFilter (const oneOccurence)

oneOccurence :: TermResults -> Bool
oneOccurence = (== 1) . totalOccurrenceCount

withLikelihoods :: [RemovalLikelihood] -> ParseResponse -> ParseResponse
withLikelihoods [] = id
withLikelihoods l = applyFilter (const $ includesLikelihood l)

ignoringPaths :: [String] -> ParseResponse -> ParseResponse
ignoringPaths xs =
    fmap (updateMatches newMatches)
  where
    newMatches = filter (not . matchesPath . tmPath)
    matchesPath p = any (`isInfixOf` p) xs

includesLikelihood :: [RemovalLikelihood] -> TermResults -> Bool
includesLikelihood l = (`elem` l) . rLikelihood . trRemoval

isClassOrModule :: TermResults -> Bool
isClassOrModule = matchRegex "^[A-Z]" . trTerm

railsSingleOkay :: TermResults -> Bool
railsSingleOkay r =
    isClassOrModule r && (controller || helper || migration)
  where
    controller = any (matchRegex "^app/controllers/") paths && matchRegex "Controller$" (trTerm r)
    helper = any (matchRegex "^app/helpers/") paths && matchRegex "Helper$" (trTerm r)
    migration = any (matchRegex "^db/migrate/") paths
    paths = tmPath <$> trMatches r

elixirSingleOkay :: TermResults -> Bool
elixirSingleOkay r =
    isAllowedTerm r allowedTerms ||
      isClassOrModule r && (view || test || migration)
  where
    migration = any (matchRegex "^priv/repo/migrations/") paths
    view = any (matchRegex "^web/views/") paths && matchRegex "View$" (trTerm r)
    test = any (matchRegex "^test/") paths && matchRegex "Test$" (trTerm r)
    allowedTerms = ["Mixfile", "__using__"]
    paths = tmPath <$> trMatches r

updateMatches :: ([TermMatch] -> [TermMatch]) -> TermMatchSet -> TermMatchSet
updateMatches fm =
    Map.map (updateMatchesWith $ fm . trMatches)
  where
    updateMatchesWith f tr = tr { trMatches = f tr }

applyFilter :: (String -> TermResults -> Bool) -> ParseResponse -> ParseResponse
applyFilter = fmap . Map.filterWithKey

isAllowedTerm :: TermResults -> [String] -> Bool
isAllowedTerm = elem . trTerm
