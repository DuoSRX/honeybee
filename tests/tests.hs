{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.ByteString.Char8
import Data.Honeybee
import Test.Hspec
import qualified Data.Map as M

main :: IO ()
main = hspec $ do

  describe "Parse integers" $ do
    it "return a BInteger" $ do
      let i = parseOnly parseBValue "i3e"
      i `shouldBe` Right (BInteger 3)

  describe "Parse list" $ do
    it "parse an empty list" $ do
      let list = parseOnly parseBValue "le"
      list `shouldBe` Right (BList [])

    it "parse an integer list" $ do
      let l = parseOnly parseBValue "li3ei12ee"
      l `shouldBe` Right (BList [BInteger 3, BInteger 12])

    it "parse a list of lists" $ do
      let l = parseOnly parseBValue "lli3eeli12ei5eee"
      l `shouldBe` Right (BList [BList [BInteger 3], BList [BInteger 12, BInteger 5]])

  describe "Parse string" $ do
    it "parse an empty string" $ do
      let s = parseOnly parseBValue "0:"
      s `shouldBe` Right (BString "")

    it "parse a string" $ do
      let s = parseOnly parseBValue "4:spam"
      s `shouldBe` Right (BString "spam")

    it "fails on invalid string length" $ do
      let s = parseOnly parseBValue "5:spam"
      s `shouldBe` Left "Failed reading: satisfyWith"

  describe "Parse dictionary" $ do
    it "parse an empty dictionary" $ do
      let d = parseOnly parseBValue "de"
      d `shouldBe` Right (BDict (M.fromList []))

    it "parse a simple dictionary" $ do
      let d = parseOnly parseBValue "d4:spaml1:a1:bee"
      d `shouldBe` Right (BDict (M.fromList [(BString "spam",BList [BString "a",BString "b"])])) 
