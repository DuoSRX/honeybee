module Data.Honeybee.Types (BValue(..))where

import qualified Data.Map as M

data BValue = BInteger Integer
            | BString String
            | BList [BValue]
            | BDict (M.Map BValue BValue)
              deriving (Show, Eq, Ord)
