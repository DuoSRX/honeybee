{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Honeybee
import Test.Hspec
import qualified Data.Map as M

main :: IO ()
main = hspec $ do

  describe "Parse integers" $ do
    it "return a BInteger" $ do
      let i = decode "i3e"
      i `shouldBe` Right (BInteger 3)

  describe "Parse list" $ do
    it "parse an empty list" $ do
      let list = decode "le"
      list `shouldBe` Right (BList [])

    it "parse an integer list" $ do
      let l = decode "li3ei12ee"
      l `shouldBe` Right (BList [BInteger 3, BInteger 12])

    it "parse a list of lists" $ do
      let l = decode "lli3eeli12ei5eee"
      l `shouldBe` Right (BList [BList [BInteger 3], BList [BInteger 12, BInteger 5]])

  describe "Parse string" $ do
    it "parse an empty string" $ do
      let s = decode "0:"
      s `shouldBe` Right (BString "")

    it "parse a string" $ do
      let s = decode "4:spam"
      s `shouldBe` Right (BString "spam")

    it "fails on invalid string length" $ do
      let s = decode "5:spam"
      s `shouldBe` Left "Failed reading: satisfyWith"

  describe "Parse dictionary" $ do
    it "parse an empty dictionary" $ do
      let d = decode "de"
      d `shouldBe` Right (BDict (M.fromList []))

    it "parse a simple dictionary" $ do
      let d = decode "d4:spaml1:a1:bee"
      d `shouldBe` Right (BDict (M.fromList [(BString "spam",BList [BString "a",BString "b"])])) 
