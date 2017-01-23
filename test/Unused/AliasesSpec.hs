module Unused.AliasesSpec where

import Data.Monoid ((<>))
import Test.Hspec
import Unused.Aliases
import Unused.ResultsClassifier.Types (TermAlias(..))
import Unused.Types (SearchTerm(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "termsAndAliases" $ do
        it "returns the terms if no aliases are provided" $
            termsAndAliases [] ["method_1", "method_2"] `shouldBe` [OriginalTerm "method_1", OriginalTerm "method_2"]

        it "adds aliases to the list of terms" $ do
            let predicateAlias = TermAlias "*?" "be_{}" ("be_" <>)
            let pluralizeAlias = TermAlias "really_*" "very_{}" ("very_" <>)

            termsAndAliases [predicateAlias, pluralizeAlias] ["awesome?", "really_cool"]
                `shouldBe` [OriginalTerm "awesome?", AliasTerm "awesome?" "be_awesome", OriginalTerm "really_cool", AliasTerm "really_cool" "very_cool"]
