{-# LANGUAGE FlexibleContexts #-}

module Unused.Regex
    ( matchRegex
    ) where

import Text.Regex.TDFA

matchRegex :: String -> String -> Bool
matchRegex = matchTest . stringToRegex

stringToRegex :: RegexMaker Regex CompOption ExecOption String => String -> Regex
stringToRegex = makeRegex
