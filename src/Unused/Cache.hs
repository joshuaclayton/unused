module Unused.Cache
    ( cached
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import System.Directory
import Data.Csv (FromRecord, ToRecord, HasHeader(..), encode, decode)
import Data.Vector (toList)
import qualified Data.ByteString.Lazy as BS
import Unused.Cache.DirectoryFingerprint (sha)

newtype CacheFileName = CacheFileName String
type Cache = ReaderT CacheFileName IO

cached :: (FromRecord a, ToRecord a) => String -> IO [a] -> IO [a]
cached context f =
    runReaderT fromCache =<< cacheFileName context
  where
    fromCache = maybe (writeCache =<< liftIO f) return =<< readCache

writeCache :: ToRecord a => [a] -> Cache [a]
writeCache [] = return []
writeCache contents = do
    liftIO $ createDirectoryIfMissing True cacheDirectory
    (CacheFileName fileName) <- ask
    liftIO $ BS.writeFile fileName $ encode contents
    return contents

readCache :: FromRecord a => Cache (Maybe [a])
readCache = do
    (CacheFileName fileName) <- ask
    exists <- liftIO $ doesFileExist fileName

    if exists
        then fmap processCsv (decode NoHeader <$> liftIO (BS.readFile fileName))
        else return Nothing
  where
    processCsv = either (const Nothing) (Just . toList)

cacheFileName :: String -> IO CacheFileName
cacheFileName context = do
    putStrLn "\n\nCalculating cache fingerprint... "
    currentSha <- sha
    return $ CacheFileName $ cacheDirectory ++ "/" ++ context ++ "-" ++ currentSha ++ ".csv"

cacheDirectory :: String
cacheDirectory = "tmp/unused"
