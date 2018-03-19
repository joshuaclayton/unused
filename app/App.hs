{-# LANGUAGE FlexibleContexts #-}

module App
    ( runProgram
    ) where

import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader (asks, liftIO, runReaderT)
import qualified Data.Bifunctor as BF
import qualified Data.Bool as B
import qualified Data.Maybe as M
import Types
import Unused.Aliases (termsAndAliases)
import Unused.CLI
       (SearchRunner(..), executeSearch, loadGitContext, renderHeader,
        withRuntime)
import qualified Unused.CLI.Views as V
import Unused.Cache (cached)
import Unused.Grouping (CurrentGrouping(..), groupedResponses)
import Unused.Parser (parseResults)
import Unused.ResponseFilter
       (ignoringPaths, withLikelihoods, withOneOccurrence)
import Unused.ResultsClassifier
       (LanguageConfiguration(..), loadAllConfigurations)
import Unused.TagsSource (loadTagsFromFile, loadTagsFromPipe)
import Unused.TermSearch
       (SearchBackend(..), SearchResults(..), SearchTerm, fromResults)
import Unused.Types (RemovalLikelihood(..), TermMatchSet)

runProgram :: Options -> IO ()
runProgram options =
    withRuntime $ either renderError return =<< runExceptT (runReaderT (runApp run) options)

run :: App ()
run = do
    terms <- termsWithAlternatesFromConfig
    liftIO $ renderHeader terms
    backend <- searchBackend
    results <- withCache . flip (executeSearch backend) terms =<< searchRunner
    printResults =<< retrieveGitContext =<< fmap (`parseResults` results) loadAllConfigs

searchBackend :: AppConfig m => m SearchBackend
searchBackend = asks oSearchBackend

termsWithAlternatesFromConfig :: App [SearchTerm]
termsWithAlternatesFromConfig =
    termsAndAliases <$> (concatMap lcTermAliases <$> loadAllConfigs) <*> calculateTagInput

renderError :: AppError -> IO ()
renderError (TagError e) = V.missingTagsFileError e
renderError (InvalidConfigError e) = V.invalidConfigError e
renderError (CacheError e) = V.fingerprintError e

retrieveGitContext :: TermMatchSet -> App TermMatchSet
retrieveGitContext tms = maybe (return tms) (liftIO . flip loadGitContext tms) =<< numberOfCommits

printResults :: TermMatchSet -> App ()
printResults tms = do
    filters <- optionFilters tms
    grouping <- groupingOptions
    formatter <- resultFormatter
    liftIO $ V.searchResults formatter $ groupedResponses grouping filters

loadAllConfigs :: App [LanguageConfiguration]
loadAllConfigs =
    either throwError return =<< BF.first InvalidConfigError <$> liftIO loadAllConfigurations

calculateTagInput :: App [String]
calculateTagInput =
    either throwError return =<<
    liftIO . fmap (BF.first TagError) . B.bool loadTagsFromFile loadTagsFromPipe =<< readFromStdIn

withCache :: IO SearchResults -> App SearchResults
withCache f = B.bool (liftIO f) (withCache' f) =<< runWithCache
  where
    withCache' :: IO SearchResults -> App SearchResults
    withCache' r =
        either (throwError . CacheError) (return . SearchResults) =<<
        liftIO (cached "term-matches" $ fmap fromResults r)

optionFilters :: AppConfig m => TermMatchSet -> m TermMatchSet
optionFilters tms = foldl (>>=) (pure tms) matchSetFilters
  where
    matchSetFilters = [singleOccurrenceFilter, likelihoodsFilter, ignoredPathsFilter]

singleOccurrenceFilter :: AppConfig m => TermMatchSet -> m TermMatchSet
singleOccurrenceFilter tms = B.bool tms (withOneOccurrence tms) <$> asks oSingleOccurrenceMatches

likelihoodsFilter :: AppConfig m => TermMatchSet -> m TermMatchSet
likelihoodsFilter tms = asks $ withLikelihoods . likelihoods <*> pure tms
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
resultFormatter = B.bool V.Column V.List . M.isJust <$> numberOfCommits
