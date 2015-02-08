module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Data/Honeybee.hs"]

