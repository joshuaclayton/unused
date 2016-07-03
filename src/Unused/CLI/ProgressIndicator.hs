module Unused.CLI.ProgressIndicator
    ( I.ProgressIndicator
    , createProgressBar
    , createSpinner
    , progressWithIndicator
    ) where

import qualified Control.Concurrent.ParallelIO as PIO
import qualified Unused.CLI.ProgressIndicator.Internal as I
import qualified Unused.CLI.ProgressIndicator.Types as I
import           Unused.CLI.Util (Color(..), installChildInterruptHandler)

createProgressBar :: I.ProgressIndicator
createProgressBar = I.ProgressBar Nothing Nothing

createSpinner :: I.ProgressIndicator
createSpinner =
    I.Spinner snapshots (length snapshots) 75000 colors Nothing
  where
    snapshots = ["⣾", "⣽", "⣻", "⢿", "⡿", "⣟", "⣯", "⣷"]
    colors = cycle [Black, Red, Yellow, Green, Blue, Cyan, Magenta]

progressWithIndicator :: Monoid b => (a -> IO b) -> I.ProgressIndicator -> [a] -> IO b
progressWithIndicator f i terms = do
    I.printPrefix i
    (tid, indicator) <- I.start i $ length terms
    installChildInterruptHandler tid
    mconcat <$> PIO.parallel (ioOps indicator) <* I.stop indicator
  where
    ioOps i' = map (\t -> f t <* I.increment i') terms
