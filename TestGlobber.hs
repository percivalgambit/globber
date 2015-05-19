module Main (main) where

import Test.Hspec

import Globber

main :: IO ()
main = hspec $ describe "Testing Globber" $ do

    describe "empty pattern" $ do
        it "matches empty string" $
            matchGlob "" "" `shouldBe` True
        it "shouldn't match non-empty string" $
            matchGlob "" "string" `shouldBe` False

    describe "question mark cases" $ do
        it "matches any single character" $
            matchGlob "?" "*" `shouldBe` True
        it "matches any character within a longer string" $
            matchGlob "?s" "xs" `shouldBe` True
        it "only matches a single character" $
            matchGlob "?" "xs" `shouldBe` False
        it "doesn't match the empty string" $
            matchGlob "?" "" `shouldBe` False
