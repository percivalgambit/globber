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

    describe "literal string cases" $ do
        it "matches a literal string" $
            matchGlob "foo" "foo" `shouldBe` True
        it "does not match a different string" $
            matchGlob "bar" "bad" `shouldBe` False
        it "does not match a longer string" $
            matchGlob "foo" "foot" `shouldBe` False
        it "does not match a shorter string" $
            matchGlob "bar" "ba" `shouldBe` False

    describe "question mark cases" $ do
        it "matches any single character" $
            matchGlob "?" "*" `shouldBe` True
        it "matches any character within a longer string" $ do
            matchGlob "?s" "xs" `shouldBe` True
            matchGlob "x?" "xs" `shouldBe` True
        it "only matches a single character" $
            matchGlob "?" "xs" `shouldBe` False
        it "doesn't match the empty string" $
            matchGlob "?" "" `shouldBe` False

    describe "star cases" $ do
        it "matches any single character" $
            matchGlob "*" "a" `shouldBe` True
        it "matches any character within a longer string" $ do
            matchGlob "*s" "xs" `shouldBe` True
            matchGlob "x*" "xs" `shouldBe` True
        it "can match a larger string" $ do
            matchGlob "*" "baz" `shouldBe` True
            matchGlob "*t" "foot" `shouldBe` True
            matchGlob "f*" "food" `shouldBe` True
        it "matches the empty string" $
            matchGlob "*" "" `shouldBe` True
        it "does not match if literal characters in pattern do not match" $ do
            matchGlob "foo*" "bard" `shouldBe` False
            matchGlob "*d" "foot" `shouldBe` False

    describe "escaped character cases" $ do
        it "can match any literal character, even a special symbol" $ do
            matchGlob "\\f" "f" `shouldBe` True
            matchGlob "\\*" "*" `shouldBe` True
            matchGlob "foo\\*" "foo*" `shouldBe` True
            matchGlob "\\foot" "foot" `shouldBe` True
        it "does not match a different character" $ do
            matchGlob "\\f" "b" `shouldBe` False
            matchGlob "\\*oo" "foo" `shouldBe` False
            matchGlob "foo\\d" "foot" `shouldBe` False
        it "does not match if there is no character after the backslash" $ do
            matchGlob "\\" "" `shouldBe` False
            matchGlob "foo\\" "foo" `shouldBe` False
