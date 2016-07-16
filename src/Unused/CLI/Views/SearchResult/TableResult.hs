module Unused.CLI.Views.SearchResult.TableResult
    ( printTable
    ) where

import           Control.Monad (forM_)
import           Unused.CLI.Util
import qualified Unused.CLI.Views.SearchResult.Internal as SR
import qualified Unused.CLI.Views.SearchResult.Types as SR
import           Unused.Types (TermResults, TermMatch(..), totalFileCount, totalOccurrenceCount)

printTable :: TermResults -> [TermMatch] -> SR.ResultsPrinter ()
printTable r ms = do
    cf <- SR.columnFormat
    let printTerm = SR.cfPrintTerm cf
    let printPath = SR.cfPrintPath cf
    let printNumber = SR.cfPrintNumber cf

    SR.liftIO $ forM_ ms $ \m -> do
        setSGR [SetColor Foreground Dull (SR.termColor r)]
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

        putStr $ "  " ++ SR.removalReason r
        putStr "\n"
