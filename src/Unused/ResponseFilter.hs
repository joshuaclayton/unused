module Unused.ResponseFilter
    ( withOneFile
    , withOneOccurrence
    , oneFile
    , oneOccurence
    , isClassOrModule
    , railsSingleOkay
    , elixirSingleOkay
    , updateMatches
    ) where

import qualified Data.Map.Strict as Map
import Unused.Regex (matchRegex)
import Unused.Types

withOneFile :: ParseResponse -> ParseResponse
withOneFile = applyFilter (const oneFile)

withOneOccurrence :: ParseResponse -> ParseResponse
withOneOccurrence = applyFilter (const oneOccurence)

oneOccurence :: TermResults -> Bool
oneOccurence = (== 1) . trTotalOccurrences

oneFile :: TermResults -> Bool
oneFile = (== 1) . trTotalFiles

isClassOrModule :: TermResults -> Bool
isClassOrModule = matchRegex "^[A-Z]" . trTerm

railsSingleOkay :: TermResults -> Bool
railsSingleOkay r =
    and [isClassOrModule r, oneFile r, oneOccurence r, controller || helper || migration]
  where
    controller = matchRegex "^app/controllers/" singlePath && matchRegex "Controller$" (trTerm r)
    helper = matchRegex "^app/helpers/" singlePath && matchRegex "Helper$" (trTerm r)
    migration = matchRegex "^db/migrate/" singlePath
    singlePath = path $ tmPath <$> trMatches r
    path (x:_) = x
    path [] = ""

elixirSingleOkay :: TermResults -> Bool
elixirSingleOkay r =
    and [isClassOrModule r, oneFile r, oneOccurence r, view || test || migration]
  where
    migration = matchRegex "^priv/repo/migrations/" singlePath
    view = matchRegex "^web/views/" singlePath && matchRegex "View$" (trTerm r)
    test = matchRegex "^test/" singlePath && matchRegex "Test$" (trTerm r)
    singlePath = path $ tmPath <$> trMatches r
    path (x:_) = x
    path [] = ""

updateMatches :: ([TermMatch] -> [TermMatch]) -> TermMatchSet -> TermMatchSet
updateMatches fm =
    Map.map (updateMatchesWith $ fm . trMatches)
  where
    updateMatchesWith f tr = tr { trMatches = f tr }

applyFilter :: (String -> TermResults -> Bool) -> ParseResponse -> ParseResponse
applyFilter = fmap . Map.filterWithKey
