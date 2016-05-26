module Unused.Util
    ( groupBy
    , stringToInt
    , readIfFileExists
    ) where

import System.Directory (doesFileExist)
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
    loop = foldl (\acc x -> acc * 10 + digitToInt x)

readIfFileExists :: String -> IO (Maybe String)
readIfFileExists path = do
    exists <- doesFileExist path

    if exists
        then Just <$> readFile path
        else return Nothing
