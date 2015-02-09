module Data.Honeybee.Types (BValue(..))where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

data BValue = BInteger Integer
            | BString B.ByteString
            | BList [BValue]
            | BDict (M.Map BValue BValue)
              deriving (Show, Eq, Ord)
