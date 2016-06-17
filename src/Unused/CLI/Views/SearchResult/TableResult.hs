module Unused.CLI.Views.SearchResult.TableResult
    ( printTable
    ) where

import Control.Monad (forM_)
import Unused.Types
import Unused.CLI.Util
import Unused.CLI.Views.SearchResult.Internal
import Unused.CLI.Views.SearchResult.Types

printTable :: TermResults -> [TermMatch] -> ResultsPrinter ()
printTable r ms = do
    cf <- columnFormat
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
