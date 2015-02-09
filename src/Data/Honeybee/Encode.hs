{-# LANGUAGE OverloadedStrings #-}
module Data.Honeybee.Encode (encode) where

import Data.Honeybee.Types
import qualified Data.Map as M

-- $setup
-- >>> :set -XOverloadedStrings

-- | Encode a BValue to a `String`
--
-- >>> encode (BString "spam")
-- "4:spam"
-- >>> encode (BList [BInteger 4, BString "spam"])
-- "li4e4:spame"
encode :: BValue -> String
encode (BString s)  = (show $ length s) ++ ":" ++ s
encode (BInteger i) = "i" ++ show i ++ "e"
encode (BList l)    = "l" ++ concatMap encode l ++ "e"
encode (BDict d)    = "d" ++ M.foldWithKey fromPair "" d ++ "e"

fromPair :: BValue -> BValue -> String -> String
fromPair k x ks = encode k ++ encode x ++ ks
