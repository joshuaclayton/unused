{-# LANGUAGE TemplateHaskell #-}

module Unused.ResultsClassifier.Config
    ( loadConfig
    , loadAllConfigurations
    ) where

import qualified Data.Bifunctor as BF
import qualified Data.ByteString as BS
import qualified Data.Either as E
import qualified Data.FileEmbed as FE
import qualified Data.Yaml as Y
import qualified System.Directory as D
import System.FilePath ((</>))
import Unused.ResultsClassifier.Types (LanguageConfiguration, ParseConfigError(..))
import Unused.Util (safeReadFile)

loadConfig :: Either String [LanguageConfiguration]
loadConfig = decodeEither defaultConfigFile

defaultConfigFile :: BS.ByteString
defaultConfigFile = $(FE.embedFile "data/config.yml")

loadAllConfigurations :: IO (Either [ParseConfigError] [LanguageConfiguration])
loadAllConfigurations = do
    homeDir <- D.getHomeDirectory
    let defaultConfig = addSourceToLeft "default config" loadConfig
    localConfig <- loadConfigFromFile ".unused.yml"
    userConfig <- loadConfigFromFile $ homeDir </> ".unused.yml"
    let (lefts, rights) = E.partitionEithers [defaultConfig, localConfig, userConfig]
    return $
        if not (null lefts)
            then Left lefts
            else Right $ concat rights

loadConfigFromFile :: String -> IO (Either ParseConfigError [LanguageConfiguration])
loadConfigFromFile path =
    either (const $ Right []) (addSourceToLeft path . decodeEither) <$> safeReadFile path

addSourceToLeft :: String -> Either String c -> Either ParseConfigError c
addSourceToLeft = BF.first . ParseConfigError

decodeEither :: Y.FromJSON a => BS.ByteString -> Either String a
decodeEither = BF.first Y.prettyPrintParseException . Y.decodeEither'
