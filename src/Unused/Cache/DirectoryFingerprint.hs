module Unused.Cache.DirectoryFingerprint
    ( FingerprintOutcome(..)
    , sha
    ) where

import Control.Monad.Reader (ReaderT, asks, liftIO, runReaderT)
import qualified Data.Char as C
import qualified Data.Maybe as M
import qualified System.Directory as D
import qualified System.Process as P
import Unused.Cache.FindArgsFromIgnoredPaths (findArgs)
import Unused.Util (safeHead, safeReadFile)

newtype MD5ExecutablePath = MD5ExecutablePath
    { toMD5String :: String
    }

type MD5Config = ReaderT MD5ExecutablePath IO

data FingerprintOutcome =
    MD5ExecutableNotFound [String]

sha :: IO (Either FingerprintOutcome String)
sha = do
    md5Executable' <- md5Executable
    case md5Executable' of
        Just exec ->
            Right . getSha <$>
            runReaderT (fileList >>= sortInput >>= md5Result) (MD5ExecutablePath exec)
        Nothing -> return $ Left $ MD5ExecutableNotFound supportedMD5Executables
  where
    getSha = takeWhile C.isAlphaNum . M.fromMaybe "" . safeHead . lines

fileList :: MD5Config String
fileList = do
    filterNamePathArgs <- liftIO $ findArgs <$> ignoredPaths
    md5exec <- asks toMD5String
    let args =
            [".", "-type", "f", "-not", "-path", "*/.git/*"] ++
            filterNamePathArgs ++ ["-exec", md5exec, "{}", "+"]
    liftIO $ P.readProcess "find" args ""

sortInput :: String -> MD5Config String
sortInput = liftIO . P.readProcess "sort" ["-k", "2"]

md5Result :: String -> MD5Config String
md5Result r = do
    md5exec <- asks toMD5String
    liftIO $ P.readProcess md5exec [] r

ignoredPaths :: IO [String]
ignoredPaths = either (const []) id <$> (fmap lines <$> safeReadFile ".gitignore")

md5Executable :: IO (Maybe String)
md5Executable = safeHead . concat <$> mapM D.findExecutables supportedMD5Executables

supportedMD5Executables :: [String]
supportedMD5Executables = ["md5", "md5sum"]
