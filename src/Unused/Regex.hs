{-# LANGUAGE FlexibleContexts #-}

module Unused.Regex
    ( matchRegex
    ) where

import Text.Regex.TDFA

matchRegex :: String -> String -> Bool
matchRegex = matchTest . stringToRegex

stringToRegex :: String -> Regex
stringToRegex = makeRegex
