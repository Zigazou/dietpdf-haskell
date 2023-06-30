-- | This module contains parsers for PDF containers (array and dictionary).
module Pdf.Object.Parser.Container
  ( arrayP
  , dictionaryP
  ) where

import           Control.Applicative            ( (<|>) )
import           Data.Binary.Parser             ( Get
                                                , label
                                                , sepBy
                                                , word8
                                                )
import qualified Data.ByteString               as BS
import           Data.Map.Strict                ( fromList )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFDictionary
                                                  , PDFName
                                                  )
                                                , mkPDFArray
                                                )
import           Pdf.Object.Parser.HexString    ( hexStringP )
import           Pdf.Object.Parser.Keyword      ( keywordP )
import           Pdf.Object.Parser.Name         ( nameP )
import           Pdf.Object.Parser.Number       ( numberP )
import           Pdf.Object.Parser.Reference    ( referenceP )
import           Pdf.Object.Parser.String       ( stringP )
import           Pdf.Object.Parser.EmptyContent ( emptyContentP )
import           Util.Ascii                     ( asciiGREATERTHANSIGN
                                                , asciiLEFTSQUAREBRACKET
                                                , asciiLESSTHANSIGN
                                                , asciiRIGHTSQUAREBRACKET
                                                )

itemP :: Get PDFObject
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
A binary parser for a PDF array.

A PDF array is a structure signaled by square brackets.

It returns a `PDFArray`.

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
arrayP :: Get PDFObject
arrayP = label "array" $ do
  word8 asciiLEFTSQUAREBRACKET
  emptyContentP
  items <- itemP `sepBy` emptyContentP
  emptyContentP
  word8 asciiRIGHTSQUAREBRACKET
  return $ mkPDFArray items

dictionaryKeyValueP :: Get (BS.ByteString, PDFObject)
dictionaryKeyValueP = do
  PDFName key <- nameP
  emptyContentP
  value <- itemP
  return (key, value)

{-|
A binary parser for a PDF dictionary.

A PDF dictionary is a structure signaled by double less-than/greater-than signs.

It returns a `PDFDictionary`.

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
dictionaryP :: Get PDFObject
dictionaryP = label "dictionary" $ do
  word8 asciiLESSTHANSIGN >> word8 asciiLESSTHANSIGN
  emptyContentP
  dictionary <- fromList <$> sepBy dictionaryKeyValueP emptyContentP
  emptyContentP
  word8 asciiGREATERTHANSIGN >> word8 asciiGREATERTHANSIGN
  return $ PDFDictionary dictionary
