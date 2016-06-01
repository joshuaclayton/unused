module Unused.CLI.Views.SearchResult
    ( searchResults
    ) where

import Control.Monad (forM_)
import Control.Arrow ((&&&))
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Unused.Types
import Unused.Grouping (Grouping(..), GroupedTerms)
import Unused.CLI.Views.SearchResult.ColumnFormatter
import Unused.CLI.Util
import qualified Unused.CLI.Views.NoResultsFound as V

type ResultsPrinter = ReaderT ColumnFormat IO

searchResults :: [GroupedTerms] -> IO ()
searchResults terms =
    runReaderT (printFormattedTerms terms) columnFormat
  where
    columnFormat = buildColumnFormatter $ termsToResults terms
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

likelihoodColor :: RemovalLikelihood -> Color
likelihoodColor High = Red
likelihoodColor Medium = Yellow
likelihoodColor Low = Green
likelihoodColor Unknown = Black
likelihoodColor NotCalculated = Magenta

printMatches :: TermResults -> [TermMatch] -> ResultsPrinter ()
printMatches r ms = do
    cf <- ask
    let printTerm = cfPrintTerm cf
    let printPath = cfPrintPath cf
    let printNumber = cfPrintNumber cf

    liftIO $ forM_ ms $ \m -> do
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
    termColor = likelihoodColor . rLikelihood . trRemoval
    removalReason = rReason . trRemoval
