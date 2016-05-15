module Unused.UtilSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Unused.Util

data Person = Person
    { pName :: String
    , pAge :: Int
    } deriving (Eq, Show)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "groupBy" $ do
        it "groups by the result of a function" $ do
            let numbers = [1..10] :: [Int]

            groupBy ((0 ==) . flip mod 2) numbers `shouldBe` [(False, [1, 3, 5, 7, 9]), (True, [2, 4, 6, 8, 10])]

        it "handles records" $ do
            let people = [Person "Jane" 10, Person "Jane" 20, Person "John" 20]

            groupBy pName people `shouldBe` [("Jane", [Person "Jane" 10, Person "Jane" 20]), ("John", [Person "John" 20])]
            groupBy pAge people `shouldBe` [(10, [Person "Jane" 10]), (20, [Person "Jane" 20, Person "John" 20])]
