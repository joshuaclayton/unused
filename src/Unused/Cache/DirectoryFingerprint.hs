module Unused.Cache.DirectoryFingerprint
    ( FingerprintOutcome(..)
    , sha
    ) where

import System.Process
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import qualified System.Directory as D
import qualified Data.Char as C
import Data.Maybe (fromMaybe)
import Unused.Cache.FindArgsFromIgnoredPaths
import Unused.Util (safeHead, safeReadFile)

type MD5Config = ReaderT String IO

data FingerprintOutcome
    = MD5ExecutableNotFound [String]

sha :: IO (Either FingerprintOutcome String)
sha = do
    md5Executable' <- md5Executable
    case md5Executable' of
        Just exec ->
            Right . getSha <$> runReaderT (fileList >>= sortInput >>= md5Result) exec
        Nothing -> return $ Left $ MD5ExecutableNotFound supportedMD5Executables
  where
    getSha = takeWhile C.isAlphaNum . fromMaybe "" . safeHead . lines

fileList :: MD5Config String
fileList = do
    filterNamePathArgs <- liftIO $ findArgs <$> ignoredPaths
    md5exec <- ask
    let args = [".", "-type", "f", "-not", "-path", "*/.git/*"] ++ filterNamePathArgs ++ ["-exec", md5exec, "{}", "+"]
    liftIO $ readProcess "find" args ""

sortInput :: String -> MD5Config String
sortInput = liftIO . readProcess "sort" ["-k", "2"]

md5Result :: String -> MD5Config String
md5Result r = do
    md5exec <- ask
    liftIO $ readProcess md5exec [] r

ignoredPaths :: IO [String]
ignoredPaths = either (const []) id <$> (fmap lines <$> safeReadFile ".gitignore")

md5Executable :: IO (Maybe String)
md5Executable =
    safeHead . concat <$> mapM D.findExecutables supportedMD5Executables

supportedMD5Executables :: [String]
supportedMD5Executables = ["md5", "md5sum"]
