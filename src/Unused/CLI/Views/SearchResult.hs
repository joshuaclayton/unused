module Unused.CLI.Views.SearchResult
    ( ResultsFormat(..)
    , searchResults
    ) where

import           Control.Arrow ((&&&))
import qualified Data.Map.Strict as Map
import           Unused.CLI.Util
import qualified Unused.CLI.Views.NoResultsFound as V
import           Unused.CLI.Views.SearchResult.ColumnFormatter
import qualified Unused.CLI.Views.SearchResult.ListResult as V
import qualified Unused.CLI.Views.SearchResult.TableResult as V
import           Unused.CLI.Views.SearchResult.Types
import           Unused.Grouping (Grouping(..), GroupedTerms)
import           Unused.Types (TermMatchSet, TermResults(..), TermMatch)

searchResults :: ResultsFormat -> [GroupedTerms] -> IO ()
searchResults format terms = do
    resetScreen
    runReaderT (printFormattedTerms terms) resultsOptions
  where
    columnFormatter = buildColumnFormatter $ termsToResults terms
    resultsOptions = ResultsOptions columnFormatter format
    termsToResults = concatMap (Map.elems . snd)

printFormattedTerms :: [GroupedTerms] -> ResultsPrinter ()
printFormattedTerms [] = liftIO V.noResultsFound
printFormattedTerms ts = mapM_ printGroupingSection ts

listFromMatchSet :: TermMatchSet -> [(String, TermResults)]
listFromMatchSet =
  Map.toList

printGroupingSection :: GroupedTerms -> ResultsPrinter ()
printGroupingSection (g, tms) = do
    liftIO $ printGrouping g
    mapM_ printTermResults $ listFromMatchSet tms

printGrouping :: Grouping -> IO ()
printGrouping NoGrouping = return ()
printGrouping g = do
    putStr "\n"
    setSGR [SetColor Foreground Vivid Black]
    setSGR [SetConsoleIntensity BoldIntensity]
    print g
    setSGR [Reset]

printTermResults :: (String, TermResults) -> ResultsPrinter ()
printTermResults =
    uncurry printMatches . (id &&& trMatches) . snd

printMatches :: TermResults -> [TermMatch] -> ResultsPrinter ()
printMatches r ms = do
    outputFormat' <- outputFormat
    case outputFormat' of
        Column -> V.printTable r ms
        List -> V.printList r ms
