module Unused.Cache
    ( cached
    ) where

import Control.Monad.Trans.Reader
import System.Directory
import Data.Csv (FromRecord, ToRecord, HasHeader(..), encode, decode)
import Data.Vector (toList)
import qualified Data.ByteString.Lazy as BS
import Unused.Cache.DirectoryFingerprint (sha)

cached :: (FromRecord a, ToRecord a) => String -> IO [a] -> IO [a]
cached context f =
    runReaderT fromCache =<< cacheFileName context
  where
    fromCache = maybe (writeCache =<< liftReaderT f) return =<< readCache

writeCache :: ToRecord a => [a] -> ReaderT String IO [a]
writeCache contents = do
    liftReaderT $ createDirectoryIfMissing True cacheDirectory
    fileName <- ask
    liftReaderT $ BS.writeFile fileName $ encode contents
    return contents

readCache :: FromRecord a => ReaderT String IO (Maybe [a])
readCache = do
    fileName <- ask
    exists <- liftReaderT $ doesFileExist fileName

    if exists
        then fmap processCsv (decode NoHeader <$> liftReaderT (BS.readFile fileName))
        else return Nothing
  where
    processCsv = either (const Nothing) (Just . toList)

cacheFileName :: String -> IO String
cacheFileName context = do
    putStrLn "\n\nCalculating cache fingerprint... "
    currentSha <- sha
    return $ cacheDirectory ++ "/" ++ context ++ "-" ++ currentSha ++ ".csv"

cacheDirectory :: String
cacheDirectory = "tmp/unused"

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT $ const m
