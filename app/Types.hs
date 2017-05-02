{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module Types
    ( Options(..)
    , AppConfig
    , AppError(..)
    , App(..)
    ) where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadReader, ReaderT, MonadIO)
import Unused.CLI (SearchRunner)
import Unused.Cache (FingerprintOutcome)
import Unused.Grouping (CurrentGrouping)
import Unused.ResultsClassifier (ParseConfigError)
import Unused.TagsSource (TagSearchOutcome)
import Unused.TermSearch (SearchBackend)
import Unused.Types (RemovalLikelihood)

data Options = Options
    { oSearchRunner :: SearchRunner
    , oSingleOccurrenceMatches :: Bool
    , oLikelihoods :: [RemovalLikelihood]
    , oAllLikelihoods :: Bool
    , oIgnoredPaths :: [String]
    , oGrouping :: CurrentGrouping
    , oWithoutCache :: Bool
    , oFromStdIn :: Bool
    , oCommitCount :: Maybe Int
    , oSearchBackend :: SearchBackend
    }

type AppConfig = MonadReader Options

data AppError
    = TagError TagSearchOutcome
    | InvalidConfigError [ParseConfigError]
    | CacheError FingerprintOutcome

newtype App a = App {
    runApp :: ReaderT Options (ExceptT AppError IO) a
} deriving (Monad, Functor, Applicative, AppConfig, MonadError AppError, MonadIO)
