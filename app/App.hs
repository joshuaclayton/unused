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
    runExceptT (runReaderT (runApp run) options) >>= either renderError return

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

retrieveGitContext :: TermMatchSet -> App TermMatchSet
retrieveGitContext tms = do
    commitCount <- numberOfCommits
    case commitCount of
        Just c -> liftIO $ loadGitContext c tms
        Nothing -> return tms

printResults :: TermMatchSet -> App ()
printResults ts = do
    filters <- optionFilters ts
    grouping <- groupingOptions
    formatter <- resultFormatter
    liftIO $ V.searchResults formatter $ groupedResponses grouping filters

loadAllConfigs :: App [LanguageConfiguration]
loadAllConfigs = do
    configs <- liftIO (B.first InvalidConfigError <$> loadAllConfigurations)
    either throwError return configs

calculateTagInput :: App [String]
calculateTagInput = do
    tags <- liftIO . fmap (B.first TagError) . loadTags =<< readFromStdIn
    either throwError return tags
  where
    loadTags b = if b then loadTagsFromPipe else loadTagsFromFile

withCache :: IO SearchResults -> App SearchResults
withCache f =
    liftIO . operateCache =<< runWithCache
  where
    operateCache b = if b then withCache' f else f
    withCache' = fmap SearchResults . cached "term-matches" . fmap fromResults

optionFilters :: AppConfig m => TermMatchSet -> m TermMatchSet
optionFilters tms = foldl (>>=) (pure tms) matchSetFilters
  where
    matchSetFilters =
        [ singleOccurrenceFilter
        , likelihoodsFilter
        , ignoredPathsFilter
        ]

singleOccurrenceFilter :: AppConfig m => TermMatchSet -> m TermMatchSet
singleOccurrenceFilter tms = do
    allowsSingleOccurrence <- oSingleOccurrenceMatches <$> ask
    return $ if allowsSingleOccurrence
        then withOneOccurrence tms
        else tms

likelihoodsFilter :: AppConfig m => TermMatchSet -> m TermMatchSet
likelihoodsFilter tms =
     withLikelihoods . likelihoods <$> ask <*> pure tms
  where
    likelihoods options
        | oAllLikelihoods options = [High, Medium, Low]
        | null $ oLikelihoods options = [High]
        | otherwise = oLikelihoods options

ignoredPathsFilter :: AppConfig m => TermMatchSet -> m TermMatchSet
ignoredPathsFilter tms = ignoringPaths . oIgnoredPaths <$> ask <*> pure tms

readFromStdIn :: AppConfig m => m Bool
readFromStdIn = oFromStdIn <$> ask

groupingOptions :: AppConfig m => m CurrentGrouping
groupingOptions = oGrouping <$> ask

searchRunner :: AppConfig m => m SearchRunner
searchRunner = oSearchRunner <$> ask

runWithCache :: AppConfig m => m Bool
runWithCache = not . oWithoutCache <$> ask

numberOfCommits :: AppConfig m => m (Maybe Int)
numberOfCommits = oCommitCount <$> ask

resultFormatter :: AppConfig m => m V.ResultsFormat
resultFormatter = do
    c <- numberOfCommits
    return $ if isJust c
        then V.List
        else V.Column
