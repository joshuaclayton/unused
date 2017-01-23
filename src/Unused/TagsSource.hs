module Unused.TagsSource
    ( TagSearchOutcome(..)
    , loadTagsFromFile
    , loadTagsFromPipe
    ) where

import qualified Control.Exception as E
import qualified Data.Bifunctor as BF
import qualified Data.List as L
import qualified Data.Text as T
import qualified System.Directory as D
import           Unused.Util (safeReadFile)

data TagSearchOutcome
    = TagsFileNotFound [String]
    | IOError E.IOException

loadTagsFromPipe :: IO (Either TagSearchOutcome [String])
loadTagsFromPipe = fmap (Right . tokensFromTags) getContents

loadTagsFromFile :: IO (Either TagSearchOutcome [String])
loadTagsFromFile = fmap (fmap tokensFromTags) tagsContent

tokensFromTags :: String -> [String]
tokensFromTags =
    filter validTokens . L.nub . tokenLocations
  where
    tokenLocations = map (token . T.splitOn "\t" . T.pack) . lines
    token = T.unpack . head

validTokens :: String -> Bool
validTokens = not . L.isPrefixOf "!_TAG"

tagsContent :: IO (Either TagSearchOutcome String)
tagsContent = D.findFile possibleTagsFileDirectories "tags" >>= eitherReadFile

eitherReadFile :: Maybe String -> IO (Either TagSearchOutcome String)
eitherReadFile Nothing = return $ Left $ TagsFileNotFound possibleTagsFileDirectories
eitherReadFile (Just path) = BF.first IOError <$> safeReadFile path

possibleTagsFileDirectories :: [String]
possibleTagsFileDirectories = [".git", "tmp", "."]
