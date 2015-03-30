{-# LANGUAGE OverloadedStrings #-}
module Data.Honeybee.Decode (decode) where

import Control.Applicative
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString.Char8
import Data.Honeybee.Types

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

-- $setup
-- >>> :set -XOverloadedStrings

-- | Parse an integer
--
-- >>> decode "i3e"
-- Right (BInteger 3)
parseInteger :: Parser BValue
parseInteger = do
  char 'i'
  num <- signed decimal
  char 'e'
  return $ BInteger num

-- | Parse a string
-- 
-- >>> decode "4:spam"
-- Right (BString "spam")
-- >>> decode "0:"
-- Right (BString "")
parseString :: Parser BValue
parseString = do
  len <- many1 digit
  char ':'
  s <- count (read len) anyChar
  return $ BString (B.pack s)

-- | Parse a list
--
-- >>> decode "l4:spam4:eggse"
-- Right (BList [BString "spam",BString "eggs"])
-- >>> decode "le"
-- Right (BList [])
parseList :: Parser BValue
parseList = do
  char 'l'
  list <- many parseBValue
  char 'e'
  return $ BList list

-- | Parse a dictionary
--
-- >>> decode "d3:cow3:moo4:spam4:eggse"
-- Right (BDict (fromList [(BString "cow",BString "moo"),(BString "spam",BString "eggs")]))
-- >>> decode "d4:spaml1:a1:bee"
-- Right (BDict (fromList [(BString "spam",BList [BString "a",BString "b"])]))
-- >>> decode "de"
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
-- Left "digit: Failed reading: satisfyWith"
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

decode :: B.ByteString -> Either String BValue
decode = parseOnly parseBValue
