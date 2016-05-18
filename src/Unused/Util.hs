module Unused.Util
    ( groupBy
    , stringToInt
    ) where

import Control.Arrow ((&&&))
import qualified Data.List as L
import Data.Function
import Data.Char (digitToInt, isDigit)

groupBy :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupBy f = map (f . head &&& id)
                   . L.groupBy ((==) `on` f)
                   . L.sortBy (compare `on` f)

stringToInt :: String -> Maybe Int
stringToInt xs
    | all isDigit xs = Just $ loop 0 xs
    | otherwise = Nothing
  where
    loop acc [] = acc
    loop acc (x:xs') = let acc' = acc * 10 + digitToInt x
                       in loop acc' xs'
