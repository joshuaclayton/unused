module Unused.Cache.FindArgsFromIgnoredPathsSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Unused.Cache.FindArgsFromIgnoredPaths

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "findArgs" $ do
        it "converts paths" $
            findArgs ["a/*", "/b/*", "c/"] `shouldBe` [ "-not", "-path", "*/a/*"
                                                      , "-not", "-path", "*/b/*"
                                                      , "-not", "-path", "*/c/*"]

        it "converts wildcards" $
            findArgs ["a/*.csv", "/b/*.csv"] `shouldBe` [ "-not", "-path", "*/a/*.csv"
                                                        , "-not", "-path", "*/b/*.csv"]

        it "filenames and paths at the same time" $
            findArgs ["/.foreman", ".bundle/"] `shouldBe` [ "-not", "-name", "*/.foreman"
                                                          , "-not", "-path", "*/.foreman/*"
                                                          , "-not", "-path", "*/.bundle/*"]
