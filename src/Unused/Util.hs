module Unused.Util
    ( groupBy
    ) where

import Data.List (nub)

groupBy :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupBy f l =
    fmap (\t -> (t, byTerm t)) uniqueTerms
  where
    byTerm t = filter ((== t) . f) l
    uniqueTerms = nub $ fmap f l
