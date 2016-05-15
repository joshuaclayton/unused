module Unused.Cache
    ( cached
    ) where

import System.Directory
import Unused.Cache.DirectoryFingerprint (sha)

cached :: IO String -> IO String
cached f = maybe (writeCache =<< f) return =<< readCache

writeCache :: String -> IO String
writeCache contents = do
    createDirectoryIfMissing True cacheDirectory
    fileName <- cacheFileName
    writeFile fileName contents
    return contents

readCache :: IO (Maybe String)
readCache = do
    putStrLn "\n\nReading from cache... "
    fileName <- cacheFileName
    exists <- doesFileExist fileName

    if exists
        then Just <$> readFile fileName
        else return Nothing

cacheFileName :: IO String
cacheFileName = do
    currentSha <- sha
    return $ cacheDirectory ++ "/" ++ currentSha ++ ".cache"

cacheDirectory :: String
cacheDirectory = "tmp/unused"
