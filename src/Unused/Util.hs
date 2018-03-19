{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Unused.Util
    ( groupBy
    , stringToInt
    , safeHead
    , safeReadFile
    ) where

import Control.Arrow ((&&&))
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as Cl8
import qualified Data.Char as C
import Data.Function (on)
import qualified Data.List as L

groupBy :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupBy f = map (f . head &&& id) . L.groupBy ((==) `on` f) . L.sortBy (compare `on` f)

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

stringToInt :: String -> Maybe Int
stringToInt xs
    | all C.isDigit xs = Just $ loop 0 xs
    | otherwise = Nothing
  where
    loop = foldl (\acc x -> acc * 10 + C.digitToInt x)

class Readable a where
    readFile' :: FilePath -> IO a

instance Readable String where
    readFile' = readFile

instance Readable C8.ByteString where
    readFile' = C8.readFile

instance Readable Cl8.ByteString where
    readFile' = Cl8.readFile

safeReadFile :: Readable s => FilePath -> IO (Either E.IOException s)
safeReadFile = E.try . readFile'
