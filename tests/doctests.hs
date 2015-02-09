module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Data/Honeybee/Encode.hs", "src/Data/Honeybee/Decode.hs"]
