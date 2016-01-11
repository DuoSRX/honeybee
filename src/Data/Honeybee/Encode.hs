{-# LANGUAGE OverloadedStrings #-}
module Data.Honeybee.Encode (encode) where

import Data.Honeybee.Types
import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

-- $setup
-- >>> :set -XOverloadedStrings

-- | Encode a BValue to a `String`
--
-- >>> encode (BString "spam")
-- "4:spam"
-- >>> encode (BList [BInteger 4, BString "spam"])
-- "li4e4:spame"
encode :: BValue -> B.ByteString
encode (BString s)  = (B.pack . show $ B.length s) <> ":" <> s
encode (BInteger i) = "i" <> B.pack (show i) <> "e"
encode (BList l)    = "l" <> foldMap encode l <> "e"
encode (BDict d)    = "d" <> M.foldWithKey fromPair "" d <> "e"

fromPair :: BValue -> BValue -> B.ByteString -> B.ByteString
fromPair k x ks = encode k <> encode x <> ks
