{-# LANGUAGE OverloadedStrings #-}

{- |
This module provides a parser for PDF indirect objects.

Any object in a PDF file may be labelled as an indirect object. This gives the
object a unique object identifier by which other objects can refer to it.

The object identifier shall consist of two parts:

- A positive integer object number. Indirect objects may be numbered
  sequentially within a PDF file, but this is not required; object numbers may
  be assigned in any arbitrary order.
- A non-negative integer generation number. In a newly created file, all
  indirect objects shall have generation numbers of 0. Nonzero generation
  numbers may be introduced when the file is later updated.

Together, the combination of an object number and a generation number shall
uniquely identify an indirect object.

The definition of an indirect object in a PDF file shall consist of its object
number and generation number (separated by white space), followed by the value
of the object bracketed between the keywords obj and endobj.

Beginning with PDF 1.5, indirect objects may reside in object streams. They are
referred to in the same way; however, their definition shall not include the
keywords obj and endobj, and their generation number shall be zero.
-}
module Pdf.Object.Parser.IndirectObject
  ( indirectObjectP
  ) where

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( when )
import           Data.Binary.Parser             ( Get
                                                , isDigit
                                                , label
                                                , manyTill
                                                , satisfy
                                                , some'
                                                , string
                                                , word8
                                                , option
                                                )
import qualified Data.ByteString               as BS
import           Data.Word                      ( Word8 )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFDictionary
                                                  , PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFName
                                                  , PDFObjectStream
                                                  , PDFXRefStream
                                                  )
                                                , isWhiteSpace
                                                , updateStream
                                                )
import           Pdf.Object.State               ( getValue
                                                , maybeQuery
                                                )
import           Pdf.Object.Parser.Container    ( arrayP
                                                , dictionaryP
                                                )
import           Pdf.Object.Parser.EmptyContent ( emptyContentP )
import           Pdf.Object.Parser.HexString    ( hexStringP )
import           Pdf.Object.Parser.Keyword      ( keywordP )
import           Pdf.Object.Parser.Name         ( nameP )
import           Pdf.Object.Parser.Number       ( numberP )
import           Pdf.Object.Parser.Reference    ( referenceP )
import           Pdf.Object.Parser.String       ( stringP )
import           Util.Ascii                     ( asciiCR
                                                , asciiLF
                                                )
import           Util.Number                    ( toNumber )

digit :: Get Word8
digit = satisfy isDigit

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

integerP :: Get [Word8]
integerP = some' digit

whiteSpaceP :: Get ()
whiteSpaceP = do
  byte <- satisfy isWhiteSpace
  when (byte == asciiCR) (word8 asciiLF)

streamWithoutCountP :: Get BS.ByteString
streamWithoutCountP = do
  string "stream"
  whiteSpaceP -- looseEndOfLineP
  stream <- manyTill
    (satisfy (const True))
    ((whiteSpaceP >> string "endstream") <|> string "endstream")
  return $! BS.pack stream

{- |
Parse a `PDFIndirectObject`.
-}
indirectObjectP :: Get PDFObject
indirectObjectP = label "indirectObject" $ do
  objectNumber <- toNumber <$> integerP
  emptyContentP
  revisionNumber <- toNumber <$> integerP
  emptyContentP
  string "obj"
  emptyContentP
  object <- itemP
  emptyContentP

  stream <- case maybeQuery (getValue "Length") object of
    Just _  -> option Nothing (Just <$> streamWithoutCountP)
    Nothing -> return Nothing

  emptyContentP
  string "endobj"

  return $ case (maybeQuery (getValue "Type") object, object, stream) of
    (Just (PDFName "XRef"), PDFDictionary dict, Just s) ->
      updateStream (PDFXRefStream objectNumber revisionNumber dict "") s
    (Just (PDFName "ObjStm"), PDFDictionary dict, Just s) ->
      updateStream (PDFObjectStream objectNumber revisionNumber dict "") s
    (_, PDFDictionary dict, Just s) -> updateStream
      (PDFIndirectObjectWithStream objectNumber revisionNumber dict "")
      s
    _anyOtherCase -> PDFIndirectObject objectNumber revisionNumber object
