module Unused.LikelihoodCalculator
    ( calculateLikelihood
    , LanguageConfiguration
    ) where

import Data.Maybe (isJust)
import Data.List (find, intercalate)
import Unused.ResultsClassifier
import Unused.Types
import Unused.ResponseFilter (autoLowLikelihood)

calculateLikelihood :: [LanguageConfiguration] -> TermResults -> TermResults
calculateLikelihood lcs r =
    r { trRemoval = uncurry Removal newLikelihood }
  where
    baseScore = totalOccurrenceCount r
    totalScore = baseScore
    newLikelihood
        | isJust firstAutoLowLikelihood = (Low, autoLowLikelihoodMessage)
        | singleNonTestUsage r && testsExist r = (High, "only the definition and corresponding tests exist")
        | doubleNonTestUsage r && testsExist r = (Medium, "only the definition and one other use, along with tests, exists")
        | totalScore < 2 = (High, "used once")
        | totalScore < 6 = (Medium, "used semi-frequently")
        | totalScore >= 6 = (Low, "used frequently")
        | otherwise = (Unknown, "could not determine likelihood")
    firstAutoLowLikelihood = find (`autoLowLikelihood` r) lcs
    autoLowLikelihoodMessage =
        case firstAutoLowLikelihood of
            Nothing -> ""
            Just lang -> languageConfirmationMessage lang

languageConfirmationMessage :: LanguageConfiguration -> String
languageConfirmationMessage lc =
    langFramework ++ ": allowed term or " ++ lowLikelihoodNames
  where
    langFramework = lcName lc
    lowLikelihoodNames = intercalate ", " $ map smName $ lcAutoLowLikelihood lc

singleNonTestUsage :: TermResults -> Bool
singleNonTestUsage = (1 ==) . oOccurrences . trAppOccurrences

doubleNonTestUsage :: TermResults -> Bool
doubleNonTestUsage = (2 ==) . oOccurrences . trAppOccurrences

testsExist :: TermResults -> Bool
testsExist = (> 0) . oOccurrences . trTestOccurrences
