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
import qualified Data.List as L
import Data.Function
import Data.Char (digitToInt, isDigit)
import qualified Data.ByteString.Lazy.Char8 as Cl
import qualified Data.ByteString.Char8 as C

groupBy :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupBy f = map (f . head &&& id)
                   . L.groupBy ((==) `on` f)
                   . L.sortBy (compare `on` f)

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

stringToInt :: String -> Maybe Int
stringToInt xs
    | all isDigit xs = Just $ loop 0 xs
    | otherwise = Nothing
  where
    loop = foldl (\acc x -> acc * 10 + digitToInt x)

class Readable a where
    readFile' :: FilePath -> IO a

instance Readable String where
    readFile' = readFile

instance Readable C.ByteString where
    readFile' = C.readFile

instance Readable Cl.ByteString where
    readFile' = Cl.readFile

safeReadFile :: Readable s => FilePath -> IO (Either E.IOException s)
safeReadFile = E.try . readFile'
