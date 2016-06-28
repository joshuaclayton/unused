{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module App
    ( Options(..)
    , runProgram
    ) where

import qualified Data.Bifunctor as B
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe (isJust)
import Data.Bool (bool)
import Unused.Grouping (CurrentGrouping(..), groupedResponses)
import Unused.Types (TermMatchSet, RemovalLikelihood(..))
import Unused.TermSearch (SearchResults(..), fromResults)
import Unused.ResponseFilter (withOneOccurrence, withLikelihoods, ignoringPaths)
import Unused.Cache
import Unused.TagsSource
import Unused.ResultsClassifier
import Unused.Aliases (termsAndAliases)
import Unused.Parser (parseResults)
import Unused.CLI (SearchRunner(..), loadGitContext, renderHeader, executeSearch, withRuntime)
import qualified Unused.CLI.Views as V

type AppConfig = MonadReader Options

data AppError
    = TagError TagSearchOutcome
    | InvalidConfigError [ParseConfigError]
    | CacheError FingerprintOutcome

newtype App a = App {
    runApp :: ReaderT Options (ExceptT AppError IO) a
} deriving (Monad, Functor, Applicative, AppConfig, MonadError AppError, MonadIO)

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
    }

runProgram :: Options -> IO ()
runProgram options = withRuntime $
    either renderError return
        =<< runExceptT (runReaderT (runApp run) options)

run :: App ()
run = do
    terms <- termsWithAlternatesFromConfig

    liftIO $ renderHeader terms
    results <- withCache . (`executeSearch` terms) =<< searchRunner

    printResults =<< retrieveGitContext =<< fmap (`parseResults` results) loadAllConfigs

termsWithAlternatesFromConfig :: App [String]
termsWithAlternatesFromConfig = do
    aliases <- concatMap lcTermAliases <$> loadAllConfigs
    terms <- calculateTagInput

    return $ termsAndAliases aliases terms

renderError :: AppError -> IO ()
renderError (TagError e) = V.missingTagsFileError e
renderError (InvalidConfigError e) = V.invalidConfigError e
renderError (CacheError e) = V.fingerprintError e

retrieveGitContext :: TermMatchSet -> App TermMatchSet
retrieveGitContext tms =
    maybe (return tms) (liftIO . flip loadGitContext tms)
        =<< numberOfCommits

printResults :: TermMatchSet -> App ()
printResults tms = do
    filters <- optionFilters tms
    grouping <- groupingOptions
    formatter <- resultFormatter
    liftIO $ V.searchResults formatter $ groupedResponses grouping filters

loadAllConfigs :: App [LanguageConfiguration]
loadAllConfigs =
    either throwError return
        =<< B.first InvalidConfigError <$> liftIO loadAllConfigurations

calculateTagInput :: App [String]
calculateTagInput =
    either throwError return
        =<< liftIO .
            fmap (B.first TagError) .
            bool loadTagsFromFile loadTagsFromPipe =<< readFromStdIn

withCache :: IO SearchResults -> App SearchResults
withCache f =
    bool (liftIO f) (withCache' f) =<< runWithCache
  where
    withCache' :: IO SearchResults -> App SearchResults
    withCache' r =
        either (throwError . CacheError) (return . SearchResults) =<<
            liftIO (cached "term-matches" $ fmap fromResults r)


optionFilters :: AppConfig m => TermMatchSet -> m TermMatchSet
optionFilters tms = foldl (>>=) (pure tms) matchSetFilters
  where
    matchSetFilters =
        [ singleOccurrenceFilter
        , likelihoodsFilter
        , ignoredPathsFilter
        ]

singleOccurrenceFilter :: AppConfig m => TermMatchSet -> m TermMatchSet
singleOccurrenceFilter tms =
    bool tms (withOneOccurrence tms) <$> asks oSingleOccurrenceMatches

likelihoodsFilter :: AppConfig m => TermMatchSet -> m TermMatchSet
likelihoodsFilter tms =
     asks $ withLikelihoods . likelihoods <*> pure tms
  where
    likelihoods options
        | oAllLikelihoods options = [High, Medium, Low]
        | null $ oLikelihoods options = [High]
        | otherwise = oLikelihoods options

ignoredPathsFilter :: AppConfig m => TermMatchSet -> m TermMatchSet
ignoredPathsFilter tms = asks $ ignoringPaths . oIgnoredPaths <*> pure tms

readFromStdIn :: AppConfig m => m Bool
readFromStdIn = asks oFromStdIn

groupingOptions :: AppConfig m => m CurrentGrouping
groupingOptions = asks oGrouping

searchRunner :: AppConfig m => m SearchRunner
searchRunner = asks oSearchRunner

runWithCache :: AppConfig m => m Bool
runWithCache = asks $ not . oWithoutCache

numberOfCommits :: AppConfig m => m (Maybe Int)
numberOfCommits = asks oCommitCount

resultFormatter :: AppConfig m => m V.ResultsFormat
resultFormatter = bool V.Column V.List . isJust <$> numberOfCommits
