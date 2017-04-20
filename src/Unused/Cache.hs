module Unused.Cache
    ( FingerprintOutcome(..)
    , cached
    ) where

import           Control.Monad.Reader (ReaderT, runReaderT, ask, liftIO)
import qualified Data.ByteString.Lazy as BS
import           Data.Csv (FromRecord, ToRecord, HasHeader(..), encode, decode)
import qualified Data.Vector as V
import qualified System.Directory as D
import           Unused.Cache.DirectoryFingerprint (FingerprintOutcome(..), sha)
import           Unused.Util (safeReadFile)

newtype CacheFileName = CacheFileName String
type Cache = ReaderT CacheFileName IO

cached :: (FromRecord a, ToRecord a) => String -> IO [a] -> IO (Either FingerprintOutcome [a])
cached cachePrefix f =
    mapM fromCache =<< cacheFileName cachePrefix
  where
    fromCache = runReaderT $ maybe (writeCache =<< liftIO f) return =<< readCache

writeCache :: ToRecord a => [a] -> Cache [a]
writeCache [] = return []
writeCache contents = do
    ensureCacheDirectoryExists
    writeContentsToCacheFile contents =<< ask
    return contents

readCache :: FromRecord a => Cache (Maybe [a])
readCache =
    either
        (const Nothing)
        (processCsv . decode NoHeader)
        <$> (readFromCache =<< ask)
  where
    processCsv = either (const Nothing) (Just . V.toList)
    readFromCache (CacheFileName fileName) = liftIO $ safeReadFile fileName

cacheFileName :: String -> IO (Either FingerprintOutcome CacheFileName)
cacheFileName context = do
    putStrLn "\n\nCalculating cache fingerprint... "
    fmap (CacheFileName . toFileName) <$> sha
  where
    toFileName s = cacheDirectory ++ "/" ++ context ++ "-" ++ s ++ ".csv"

cacheDirectory :: String
cacheDirectory = "tmp/unused"

ensureCacheDirectoryExists :: Cache ()
ensureCacheDirectoryExists =
    liftIO $ D.createDirectoryIfMissing True cacheDirectory

writeContentsToCacheFile :: ToRecord a => [a] -> CacheFileName -> Cache ()
writeContentsToCacheFile contents (CacheFileName fileName) =
    liftIO $ BS.writeFile fileName $ encode contents
