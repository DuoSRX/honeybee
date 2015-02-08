{-# LANGUAGE OverloadedStrings #-}
module Bencoding (parseBValue, BValue(..)) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import Data.Word

import qualified Data.Map as M

data BValue = BInteger Integer
            | BString String
            | BList [BValue]
            | BDict (M.Map BValue BValue)
              deriving (Show, Eq, Ord)

-- $setup
-- >>> :set -XOverloadedStrings

-- | Parse an integer
--
-- >>> parseOnly parseInteger "i3e"
-- Right (BInteger 3)
parseInteger :: Parser BValue
parseInteger = do
  char 'i'
  num <- many1 digit
  char 'e'
  return $ BInteger (read num)

-- | Parse a string
-- 
-- >>> parseOnly parseString "4:spam"
-- Right (BString "spam")
-- >>> parseOnly parseString "0:"
-- Right (BString "")
parseString :: Parser BValue
parseString = do
  len <- many1 digit
  char ':'
  s <- count (read len) anyChar
  return $ BString s

-- | Parse a list
--
-- >>> parseOnly parseList "l4:spam4:eggse"
-- Right (BList [BString "spam",BString "eggs"])
-- >>> parseOnly parseList "le"
-- Right (BList [])
parseList :: Parser BValue
parseList = do
  char 'l'
  list <- many parseBValue
  char 'e'
  return $ BList list

-- | Parse a dictionary
-- 
-- >>> parseOnly parseDict "d3:cow3:moo4:spam4:eggse"
-- Right (BDict (fromList [(BString "cow",BString "moo"),(BString "spam",BString "eggs")]))
-- >>> parseOnly parseDict "d4:spaml1:a1:bee"
-- Right (BDict (fromList [(BString "spam",BList [BString "a",BString "b"])]))
-- >>> parseOnly parseDict "de"
-- Right (BDict (fromList []))
parseDict :: Parser BValue
parseDict = do
  char 'd'
  pairs <- many parseDictPair
  char 'e'
  return . BDict $ M.fromList pairs

-- | Parse a dictionary key/value pair
--
-- >>> parseOnly parseDictPair "3:cowi2e"
-- Right (BString "cow",BInteger 2)
-- >>> parseOnly parseDictPair "i10ei2e"
-- Left "Failed reading: satisfyWith"
parseDictPair :: Parser (BValue, BValue)
parseDictPair = do
  key <- parseString
  value <- parseBValue
  return (key, value)

-- | Parse any BEncoded value
parseBValue :: Parser BValue
parseBValue = parseInteger
              <|> parseString
              <|> parseList
              <|> parseDict
