module Unused.CLI.Views.SearchResult
    ( searchResults
    ) where

import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Unused.Types
import Unused.Grouping (Grouping(..), GroupedTerms)
import Unused.CLI.Views.SearchResult.ColumnFormatter
import Unused.CLI.Util
import qualified Unused.CLI.Views.NoResultsFound as V

searchResults :: [GroupedTerms] -> IO ()
searchResults terms =
    printFormattedTerms columnFormat terms
  where
    allSets = listFromMatchSet =<< map snd terms
    allResults = map snd allSets
    columnFormat = buildColumnFormatter allResults

printFormattedTerms :: ColumnFormat -> [GroupedTerms] -> IO ()
printFormattedTerms _ [] = V.noResultsFound
printFormattedTerms cf ts = mapM_ (printGroupingSection cf) ts

listFromMatchSet :: TermMatchSet -> [(String, TermResults)]
listFromMatchSet =
  Map.toList

printGroupingSection :: ColumnFormat -> GroupedTerms -> IO ()
printGroupingSection cf (g, tms) = do
    printGrouping g
    mapM_ (printTermResults cf) $ listFromMatchSet tms

printGrouping :: Grouping -> IO ()
printGrouping NoGrouping = return ()
printGrouping g = do
    putStr "\n"
    setSGR [SetColor Foreground Vivid Black]
    setSGR [SetConsoleIntensity BoldIntensity]
    print g
    setSGR [Reset]

printTermResults :: ColumnFormat -> (String, TermResults) -> IO ()
printTermResults cf (_, results) =
    printMatches cf results $ trMatches results

likelihoodColor :: RemovalLikelihood -> Color
likelihoodColor High = Red
likelihoodColor Medium = Yellow
likelihoodColor Low = Green
likelihoodColor Unknown = Black
likelihoodColor NotCalculated = Magenta

printMatches :: ColumnFormat -> TermResults -> [TermMatch] -> IO ()
printMatches cf r ms =
    forM_ ms $ \m -> do
        setSGR [SetColor Foreground Dull (termColor r)]
        setSGR [SetConsoleIntensity NormalIntensity]
        putStr $ "     " ++ printTerm (tmTerm m)
        setSGR [Reset]

        setSGR [SetColor Foreground Vivid Cyan]
        setSGR [SetConsoleIntensity NormalIntensity]
        putStr $ "  " ++ printNumber (totalFileCount r) ++ ", " ++ printNumber (totalOccurrenceCount r)
        setSGR [Reset]

        setSGR [SetColor Foreground Dull Cyan]
        setSGR [SetConsoleIntensity FaintIntensity]
        putStr $ "  " ++ printPath (tmPath m)
        setSGR [Reset]

        putStr $ "  " ++ removalReason r
        putStr "\n"
  where
    printTerm = cfPrintTerm cf
    printPath = cfPrintPath cf
    printNumber = cfPrintNumber cf
    termColor = likelihoodColor . rLikelihood . trRemoval
    removalReason = rReason . trRemoval
