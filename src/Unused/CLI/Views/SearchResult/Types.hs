module Unused.CLI.Views.SearchResult.Types
    ( ResultsOptions(..)
    , ResultsFormat(..)
    , ResultsPrinter
    , ColumnFormat(..)
    , columnFormat
    , outputFormat
    , R.runReaderT
    , M.liftIO
    ) where

import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.IO.Class as M
import Unused.CLI.Views.SearchResult.ColumnFormatter

data ResultsOptions = ResultsOptions
    { roColumnFormat :: ColumnFormat
    , roOutputFormat :: ResultsFormat
    }

data ResultsFormat = Column | List
type ResultsPrinter = R.ReaderT ResultsOptions IO

columnFormat :: ResultsPrinter ColumnFormat
columnFormat = roColumnFormat <$> R.ask

outputFormat :: ResultsPrinter ResultsFormat
outputFormat = roOutputFormat <$> R.ask
