-- | This module contains parsers for GFX containers (array and dictionary).
module Pdf.Graphics.Parser.Container
  ( arrayP
  , dictionaryP
  , dictionaryKeyValueP
  ) where

import Control.Applicative ((<|>))

import Data.Binary.Parser (Get, label, sepBy, word8)
import Data.ByteString qualified as BS
import Data.Map.Strict (fromList)

import Pdf.Graphics.Object (GFXObject (GFXDictionary, GFXName), mkGFXArray)
import Pdf.Graphics.Parser.EmptyContent (emptyContentP)
import Pdf.Graphics.Parser.HexString (hexStringP)
import Pdf.Graphics.Parser.Keyword (keywordP)
import Pdf.Graphics.Parser.Name (nameP)
import Pdf.Graphics.Parser.Number (numberP)
import Pdf.Graphics.Parser.Reference (referenceP)
import Pdf.Graphics.Parser.String (stringP)

import Util.Ascii
    ( asciiGREATERTHANSIGN
    , asciiLEFTSQUAREBRACKET
    , asciiLESSTHANSIGN
    , asciiRIGHTSQUAREBRACKET
    )

itemP :: Get GFXObject
itemP =
  nameP
    <|> stringP
    <|> referenceP
    <|> numberP
    <|> keywordP
    <|> hexStringP
    <|> arrayP
    <|> dictionaryP

{-|
A binary parser for a GFX array.

A GFX array is a structure signaled by square brackets.

It returns a `GFXArray`.

An array may contain any number of the following items:

- name
- string
- reference
- number
- keyword
- hexString
- array
- dictionary
-}
arrayP :: Get GFXObject
arrayP = label "arrayG" $ do
  word8 asciiLEFTSQUAREBRACKET
  emptyContentP
  items <- itemP `sepBy` emptyContentP
  emptyContentP
  word8 asciiRIGHTSQUAREBRACKET
  return $ mkGFXArray items

{- |
A binary parser for a key (`GFXName`) value (`GFXObject`) pair.

A `List` of these key-value pairs makes a `GFXDictionary`.
-}
dictionaryKeyValueP :: Get (BS.ByteString, GFXObject)
dictionaryKeyValueP = label "keyvalueG" $ do
  GFXName key <- nameP
  emptyContentP
  value <- itemP
  return (key, value)

{-|
A binary parser for a GFX dictionary.

A GFX dictionary is a structure signaled by double less-than/greater-than signs.

It returns a `GFXDictionary`.

A dictionary may contain any number of key-value pairs of the following items:

- name
- string
- reference
- number
- keyword
- hexString
- array
- dictionary
-}
dictionaryP :: Get GFXObject
dictionaryP = label "dictionaryG" $ do
  word8 asciiLESSTHANSIGN >> word8 asciiLESSTHANSIGN
  emptyContentP
  dictionary <- fromList <$> sepBy dictionaryKeyValueP emptyContentP
  emptyContentP
  word8 asciiGREATERTHANSIGN >> word8 asciiGREATERTHANSIGN
  return $ GFXDictionary dictionary
