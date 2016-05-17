module Unused.Util
    ( groupBy
    ) where

import Control.Arrow ((&&&))
import qualified Data.List as L
import Data.Function

groupBy :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupBy f = map (f . head &&& id)
                   . L.groupBy ((==) `on` f)
                   . L.sortBy (compare `on` f)
