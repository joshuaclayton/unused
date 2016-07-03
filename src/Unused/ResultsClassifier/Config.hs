module Unused.ResultsClassifier.Config
    ( loadConfig
    , loadAllConfigurations
    ) where

import qualified Data.Yaml as Y
import qualified Data.Either as E
import qualified Data.Bifunctor as B
import System.FilePath ((</>))
import System.Directory (getHomeDirectory)
import Paths_unused (getDataFileName)
import Unused.ResultsClassifier.Types (LanguageConfiguration, ParseConfigError(..))
import Unused.Util (safeReadFile)

loadConfig :: IO (Either String [LanguageConfiguration])
loadConfig = do
    configFileName <- getDataFileName ("data" </> "config.yml")

    either
        (const $ Left "default config not found")
        Y.decodeEither
        <$> safeReadFile configFileName

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
    file <- safeReadFile path
    return $ case file of
        Left _ -> Right []
        Right body -> addSourceToLeft path $ Y.decodeEither body

addSourceToLeft :: String -> Either String c -> Either ParseConfigError c
addSourceToLeft source = B.first (ParseConfigError source)
