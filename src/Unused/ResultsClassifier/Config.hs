module Unused.ResultsClassifier.Config
    ( loadConfig
    ) where

import qualified Data.Yaml as Y
import qualified Data.ByteString as BS
import System.FilePath ((</>))
import Paths_unused (getDataFileName)
import Unused.ResultsClassifier.Types (LanguageConfiguration)

loadConfig :: IO (Either String [LanguageConfiguration])
loadConfig = Y.decodeEither <$> readConfig

readConfig :: IO BS.ByteString
readConfig = getDataFileName ("data" </> "config.yml") >>= BS.readFile
