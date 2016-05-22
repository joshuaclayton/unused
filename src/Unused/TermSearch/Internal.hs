module Unused.TermSearch.Internal
    ( commandLineOptions
    ) where

import Unused.Regex

commandLineOptions :: String -> [String]
commandLineOptions t =
    case regexSafeTerm t of
        True -> ["(\\W|^)" ++ t ++ "(\\W|$)", ".", "-c", "--ackmate"]
        False -> [t, ".", "-c", "-Q", "--ackmate"]

regexSafeTerm :: String -> Bool
regexSafeTerm =
    matchRegex "^[[:word:]]+$"
