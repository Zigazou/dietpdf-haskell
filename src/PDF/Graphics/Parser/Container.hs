{-|
Parsers for PDF graphics stream containers

This module provides binary parsers for PDF graphics stream container objects.
Containers include arrays and dictionaries, which are composite data structures
that may contain nested objects of various types.
-}
module PDF.Graphics.Parser.Container
  ( arrayP
  , dictionaryP
  , dictionaryKeyValueP
  ) where

import Control.Applicative ((<|>))

import Data.Binary.Parser (Get, label, sepBy, word8)
import Data.ByteString (ByteString)
import Data.Map.Strict (fromList)
import Data.PDF.GFXObject (GFXObject (GFXDictionary, GFXName), mkGFXArray)

import PDF.Graphics.Parser.EmptyContent (emptyContentP)
import PDF.Graphics.Parser.HexString (hexStringP)
import PDF.Graphics.Parser.Keyword (keywordP)
import PDF.Graphics.Parser.Name (nameP)
import PDF.Graphics.Parser.Number (numberP)
import PDF.Graphics.Parser.Reference (referenceP)
import PDF.Graphics.Parser.String (stringP)

import Util.Ascii
    ( asciiGREATERTHANSIGN
    , asciiLEFTSQUAREBRACKET
    , asciiLESSTHANSIGN
    , asciiRIGHTSQUAREBRACKET
    )

{-|
Parse a graphics stream object item.

An item can be any of the following graphics object types:

- name
- string
- reference
- number
- keyword
- hex string
- array
- dictionary

The parser tries each item type in sequence using alternative operators,
returning the first successful parse.
-}
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
Parse a graphics stream array.

A graphics array is a composite data structure delimited by square brackets.
Arrays may contain zero or more items of any supported graphics object type,
separated by whitespace or other empty content.

Supported item types:

- name
- string
- reference
- number
- keyword
- hex string
- array (nested)
- dictionary (nested)

Returns a graphics array object containing the parsed items.
-}
arrayP :: Get GFXObject
arrayP = label "arrayG" $ do
  word8 asciiLEFTSQUAREBRACKET
  emptyContentP
  items <- itemP `sepBy` emptyContentP
  emptyContentP
  word8 asciiRIGHTSQUAREBRACKET
  return $ mkGFXArray items

{-|
Parse a key-value pair for a graphics dictionary.

A key-value pair consists of a name (which becomes the dictionary key) followed
by a graphics object (which becomes the dictionary value). The key and value are
separated by whitespace or other empty content.

Multiple key-value pairs can be parsed sequentially to construct a complete
graphics dictionary.
-}
dictionaryKeyValueP :: Get (ByteString, GFXObject)
dictionaryKeyValueP = label "keyvalueG" $ do
  GFXName key <- nameP
  emptyContentP
  value <- itemP
  return (key, value)

{-|
Parse a graphics stream dictionary.

A graphics dictionary is a composite data structure delimited by double angle
brackets (@<<@ and @>>@). Dictionaries are key-value stores where keys are names
and values are graphics objects.

Dictionaries may contain zero or more key-value pairs, where each pair is
separated by whitespace or other empty content.

Supported value types:

- name
- string
- reference
- number
- keyword
- hex string
- array
- dictionary (nested)

Returns a graphics dictionary object containing the parsed key-value pairs.
-}
dictionaryP :: Get GFXObject
dictionaryP = label "dictionaryG" $ do
  word8 asciiLESSTHANSIGN >> word8 asciiLESSTHANSIGN
  emptyContentP
  dictionary <- fromList <$> sepBy dictionaryKeyValueP emptyContentP
  emptyContentP
  word8 asciiGREATERTHANSIGN >> word8 asciiGREATERTHANSIGN
  return $ GFXDictionary dictionary
