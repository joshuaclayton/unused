{-# LANGUAGE OverloadedStrings #-}

module Unused.TagsSource
    ( TagSearchOutcome(..)
    , loadTagsFromFile
    , loadTagsFromPipe
    ) where

import Data.List (nub)
import System.Directory (findFile)
import Unused.Regex (matchRegex)
import qualified Data.Text as T

data TagSearchOutcome
    = TagsFileNotFound [String]

loadTagsFromPipe :: IO (Either TagSearchOutcome [String])
loadTagsFromPipe = fmap (Right . tokensFromTags) getContents

loadTagsFromFile :: IO (Either TagSearchOutcome [String])
loadTagsFromFile = fmap (fmap tokensFromTags) tagsContent

tokensFromTags :: String -> [String]
tokensFromTags =
    filter tagRemovalRegex . nub . map token . tokenLocations
  where
    tokenLocations = map (T.splitOn "\t" . T.pack) . lines
    token = T.unpack . head

tagRemovalRegex :: String -> Bool
tagRemovalRegex = not . matchRegex "^!_TAG"

tagsContent :: IO (Either TagSearchOutcome String)
tagsContent = findFile possibleTagsFileDirectories "tags" >>= eitherReadFile

eitherReadFile :: Maybe String -> IO (Either TagSearchOutcome String)
eitherReadFile Nothing = return $ Left $ TagsFileNotFound possibleTagsFileDirectories
eitherReadFile (Just path) = Right <$> readFile path

possibleTagsFileDirectories :: [String]
possibleTagsFileDirectories = [".git", "tmp", "."]
