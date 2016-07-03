module Unused.ResultsClassifier.Config
    ( loadConfig
    , loadAllConfigurations
    ) where

import qualified Data.Bifunctor as BF
import qualified Data.Either as E
import qualified Data.Yaml as Y
import qualified Paths_unused as Paths
import qualified System.Directory as D
import           System.FilePath ((</>))
import           Unused.ResultsClassifier.Types (LanguageConfiguration, ParseConfigError(..))
import           Unused.Util (safeReadFile)

loadConfig :: IO (Either String [LanguageConfiguration])
loadConfig = do
    configFileName <- Paths.getDataFileName ("data" </> "config.yml")

    either
        (const $ Left "default config not found")
        Y.decodeEither
        <$> safeReadFile configFileName

loadAllConfigurations :: IO (Either [ParseConfigError] [LanguageConfiguration])
loadAllConfigurations = do
    homeDir <- D.getHomeDirectory

    defaultConfig <- addSourceToLeft "default config" <$> loadConfig
    localConfig <- loadConfigFromFile ".unused.yml"
    userConfig <- loadConfigFromFile $ homeDir </> ".unused.yml"

    let (lefts, rights) = E.partitionEithers [defaultConfig, localConfig, userConfig]

    return $ if not (null lefts)
        then Left lefts
        else Right $ concat rights

loadConfigFromFile :: String -> IO (Either ParseConfigError [LanguageConfiguration])
loadConfigFromFile path =
    either
        (const $ Right [])
        (addSourceToLeft path . Y.decodeEither)
        <$> safeReadFile path

addSourceToLeft :: String -> Either String c -> Either ParseConfigError c
addSourceToLeft = BF.first . ParseConfigError
