{-|
Parsers for PDF container objects (arrays and dictionaries)

This module provides binary parsers for PDF container objects: arrays and
dictionaries. These are composite data structures that can contain nested
objects of various types.
-}
module PDF.Object.Parser.Container
  ( arrayP
  , dictionaryP
  ) where

import Control.Applicative ((<|>))

import Data.Binary.Parser (Get, label, sepBy, word8)
import Data.ByteString (ByteString)
import Data.Map.Strict (fromList)

import PDF.Object.Object (PDFObject (PDFDictionary, PDFName), mkPDFArray)
import PDF.Object.Parser.EmptyContent (emptyContentP)
import PDF.Object.Parser.HexString (hexStringP)
import PDF.Object.Parser.Keyword (keywordP)
import PDF.Object.Parser.Name (nameP)
import PDF.Object.Parser.Number (numberP)
import PDF.Object.Parser.Reference (referenceP)
import PDF.Object.Parser.String (stringP)

import Util.Ascii
    ( asciiGREATERTHANSIGN
    , asciiLEFTSQUAREBRACKET
    , asciiLESSTHANSIGN
    , asciiRIGHTSQUAREBRACKET
    )

{-|
Parse a PDF object that may appear as an item in a container.

An item can be any of the following PDF object types:

- name object
- string object (literal string)
- reference to an indirect object
- number (integer or real)
- keyword
- hexadecimal string
- array (nested)
- dictionary (nested)

The parser tries each item type in sequence using alternative operators,
returning the first successful parse.
-}
itemP :: Get PDFObject
itemP = nameP
    <|> stringP
    <|> referenceP
    <|> numberP
    <|> keywordP
    <|> hexStringP
    <|> arrayP
    <|> dictionaryP

{-|
Parse a PDF array object.

A PDF array is a composite data structure delimited by square brackets (@[@ and
@]@). Arrays may contain zero or more items of any supported object type,
separated by whitespace or other empty content (comments).

Supported item types:

- name object
- string object
- reference
- number
- keyword
- hex string
- array (nested)
- dictionary (nested)

Returns a PDF array object containing the parsed items in order.
-}
arrayP :: Get PDFObject
arrayP = label "array" $ do
  word8 asciiLEFTSQUAREBRACKET
  emptyContentP
  items <- itemP `sepBy` emptyContentP
  emptyContentP
  word8 asciiRIGHTSQUAREBRACKET
  return $ mkPDFArray items

{-|
Parse a key-value pair for a PDF dictionary.

A key-value pair consists of a name object (which becomes the dictionary key)
followed by a PDF object (which becomes the dictionary value). The key and value
are separated by whitespace or other empty content (comments).

Multiple key-value pairs parsed sequentially form the entries of a complete PDF
dictionary.

Returns a tuple of the parsed key (as a bytestring) and its associated value.
-}
dictionaryKeyValueP :: Get (ByteString, PDFObject)
dictionaryKeyValueP = do
  PDFName key <- nameP
  emptyContentP
  value <- itemP
  return (key, value)

{-|
Parse a PDF dictionary object.

A PDF dictionary is a composite data structure delimited by double angle
brackets (@<<@ and @>>@). Dictionaries are associative arrays (maps) with name
keys and object values.

A dictionary may contain zero or more key-value pairs, where each pair is
separated by whitespace or other empty content (comments). Key-value pairs are
stored in ascending order by key name.

Supported value types:

- name object
- string object
- reference
- number
- keyword
- hex string
- array
- dictionary (nested)

Returns a PDF dictionary object containing the parsed key-value pairs.
-}
dictionaryP :: Get PDFObject
dictionaryP = label "dictionary" $ do
  word8 asciiLESSTHANSIGN >> word8 asciiLESSTHANSIGN
  emptyContentP
  dictionary <- fromList <$> sepBy dictionaryKeyValueP emptyContentP
  emptyContentP
  word8 asciiGREATERTHANSIGN >> word8 asciiGREATERTHANSIGN
  return $ PDFDictionary dictionary
