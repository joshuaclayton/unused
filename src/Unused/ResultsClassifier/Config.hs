module Unused.ResultsClassifier.Config
    ( loadConfig
    , loadAllConfigurations
    ) where

import qualified Data.Yaml as Y
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Either as E
import qualified Data.Bifunctor as B
import System.FilePath ((</>))
import System.Directory (getHomeDirectory, doesFileExist)
import Paths_unused (getDataFileName)
import Unused.ResultsClassifier.Types (LanguageConfiguration, ParseConfigError(..))
import Unused.Util (readIfFileExists)

loadConfig :: IO (Either String [LanguageConfiguration])
loadConfig = either Left Y.decodeEither <$> readConfig

loadAllConfigurations :: IO (Either [ParseConfigError] [LanguageConfiguration])
loadAllConfigurations = do
    homeDir <- getHomeDirectory

    defaultConfig <- addSourceToLeft "default config" <$> loadConfig
    localConfig <- loadConfigFromFile ".unused.yml"
    userConfig <- loadConfigFromFile $ homeDir </> ".unused.yml"

    let (lefts, rights) = E.partitionEithers [defaultConfig, localConfig, userConfig]

    if not (null lefts)
        then return $ Left lefts
        else return $ Right $ concat rights

loadConfigFromFile :: String -> IO (Either ParseConfigError [LanguageConfiguration])
loadConfigFromFile path = do
    file <- fmap C.pack <$> readIfFileExists path
    return $ case file of
        Nothing -> Right []
        Just body -> addSourceToLeft path $ Y.decodeEither body

addSourceToLeft :: String -> Either String c -> Either ParseConfigError c
addSourceToLeft source = B.first (ParseConfigError source)

readConfig :: IO (Either String BS.ByteString)
readConfig = do
    configFileName <- getDataFileName ("data" </> "config.yml")
    exists <- doesFileExist configFileName

    if exists
        then Right <$> BS.readFile configFileName
        else return $ Left "default config not found"
