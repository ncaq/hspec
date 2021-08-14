{-# LANGUAGE ScopedTypeVariables #-}
module Test.Hspec.Core.Formatters.DiffSpec (spec) where

import           Prelude ()
import           Helper hiding (First)
import           Data.Char

import           Test.Hspec.Core.Formatters.Diff as Diff

dropQuotes :: String -> String
dropQuotes = init . tail

spec :: Spec
spec = do
  describe "smartDiff" $ do
    it "parses back multi-line string literals" $ do
      smartDiff (show "foo\nbar\nbaz\n") (show "foo\nbaz\n") `shouldBe` [Both "foo\n", First "bar\n", Both "baz\n"]

    it "does not parse back string literals that span a single line" $ do
      smartDiff (show "foo\n") (show "bar\n") `shouldBe` [Both "\"", First "foo", Second "bar", Both "\\n\""]

    it "does not parse back string literals if the resulting diff would contain chunks consisting of a single newline" $ do
      smartDiff (show "foo\nbar  \nbaz\n") (show "foo\nbar\n  baz\n") `shouldBe` [Both "\"foo\\nbar", Second "\\n", Both "  ", First "\\n", Both "baz\\n\""]

  describe "partition" $ do
    context "with a single shown Char" $ do
      it "never partitions a character escape" $ do
        property $ \ (c :: Char) -> partition (show c) `shouldBe` ["'", dropQuotes (show c), "'"]

    context "with a shown String" $ do
      it "puts backslash-escaped characters into separate chunks" $ do
        partition (show "foo\nbar") `shouldBe` ["\"", "foo", "\\n", "bar", "\""]

      it "puts *arbitrary* backslash-escaped characters into separate chunks" $ do
        property $ \ xs c ys ->
          let
            char = dropQuotes (show [c])
            isEscaped = length char > 1
            escape = tail char
            sep = case ys of
              x : _ | all isDigit escape && isDigit x || escape == "SO" && x == 'H' -> ["\\&"]
              _ -> []
            actual = partition (show (xs ++ c : ys))
            expected = partition (init $ show xs) ++ [char] ++ sep ++ partition (tail $ show ys)
          in isEscaped ==> actual `shouldBe` expected

  describe "breakList" $ do
    context "with a list where the predicate matches at the beginning and the end" $ do
      it "breaks the list into pieces" $ do
        breakList isAlphaNum "foo bar  baz" `shouldBe` ["foo", " ", "bar", " ", " ", "baz"]

    context "with a list where the predicate does not match at the beginning and the end" $ do
      it "breaks the list into pieces" $ do
        breakList isAlphaNum "  foo bar  baz  " `shouldBe` [" ", " ", "foo", " ", "bar", " ", " ", "baz", " ", " "]
