module Unused.CLI.ProgressIndicator
    ( ProgressIndicator
    , createProgressBar
    , createSpinner
    , progressWithIndicator
    ) where

import Unused.CLI.Util
import Unused.CLI.ProgressIndicator.Types
import Unused.CLI.ProgressIndicator.Internal

createProgressBar :: ProgressIndicator
createProgressBar = ProgressBar Nothing Nothing

createSpinner :: ProgressIndicator
createSpinner =
    Spinner snapshots (length snapshots) 75000 colors Nothing
  where
    snapshots = ["⣾", "⣽", "⣻", "⢿", "⡿", "⣟", "⣯", "⣷"]
    colors = cycle [Black, Red, Yellow, Green, Blue, Cyan, Magenta]

progressWithIndicator :: (a -> IO [b]) -> ProgressIndicator -> [a] -> IO [b]
progressWithIndicator f i terms = do
    printPrefix i
    indicator <- start i $ length terms
    concat <$> mapM (\t -> f t <* increment indicator) terms <* stop indicator
