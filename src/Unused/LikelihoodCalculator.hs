module Unused.LikelihoodCalculator
    ( calculateLikelihood
    , LanguageConfiguration
    ) where

import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Unused.ResponseFilter as RF
import           Unused.ResultsClassifier (LanguageConfiguration(..), LowLikelihoodMatch(..))
import           Unused.Types (TermResults(..), Occurrences(..), RemovalLikelihood(..), Removal(..), totalOccurrenceCount)

calculateLikelihood :: [LanguageConfiguration] -> TermResults -> TermResults
calculateLikelihood lcs r =
    r { trRemoval = uncurry Removal newLikelihood }
  where
    newLikelihood
        | M.isJust firstAutoLowLikelihood = (Low, autoLowLikelihoodMessage)
        | singleNonTestUsage r && testsExist r = (High, "only the definition and corresponding tests exist")
        | doubleNonTestUsage r && testsExist r = (Medium, "only the definition and one other use, along with tests, exists")
        | totalScore < 2 = (High, "occurs once")
        | totalScore < 6 = (Medium, "used semi-frequently")
        | totalScore >= 6 = (Low, "used frequently")
        | otherwise = (Unknown, "could not determine likelihood")
    totalScore = totalOccurrenceCount r
    firstAutoLowLikelihood = L.find (`RF.autoLowLikelihood` r) lcs
    autoLowLikelihoodMessage = maybe "" languageConfirmationMessage firstAutoLowLikelihood

languageConfirmationMessage :: LanguageConfiguration -> String
languageConfirmationMessage lc =
    langFramework ++ ": allowed term or " ++ lowLikelihoodNames
  where
    langFramework = lcName lc
    lowLikelihoodNames = L.intercalate ", " $ map smName $ lcAutoLowLikelihood lc

singleNonTestUsage :: TermResults -> Bool
singleNonTestUsage = (1 ==) . oOccurrences . trAppOccurrences

doubleNonTestUsage :: TermResults -> Bool
doubleNonTestUsage = (2 ==) . oOccurrences . trAppOccurrences

testsExist :: TermResults -> Bool
testsExist = (> 0) . oOccurrences . trTestOccurrences
