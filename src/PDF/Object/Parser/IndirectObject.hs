{-|
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
module PDF.Object.Parser.IndirectObject
  ( indirectObjectP
  ) where

import Control.Applicative ((<|>))

import Data.Binary.Parser
    ( Get
    , getByteString
    , isDigit
    , label
    , manyTill
    , option
    , satisfy
    , skipMany
    , some'
    , string
    , word8
    )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)

import PDF.Object.Object
    ( PDFObject (PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNumber, PDFObjectStream, PDFXRefStream)
    , isWhiteSpace
    , updateStream
    )
import PDF.Object.Parser.Container (arrayP, dictionaryP)
import PDF.Object.Parser.EmptyContent (emptyContentP)
import PDF.Object.Parser.HexString (hexStringP)
import PDF.Object.Parser.Keyword (keywordP)
import PDF.Object.Parser.Name (nameP)
import PDF.Object.Parser.Number (numberP)
import PDF.Object.Parser.Reference (referenceP)
import PDF.Object.Parser.String (stringP)
import PDF.Object.State (getValue, maybeQuery)

import Util.Ascii (asciiCR, asciiLF)
import Util.Number (toNumber)

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

{-|
Eats an end of line in the case of stream content.

From PDF specifications:

"The keyword stream that follows the stream dictionary shall be followed by an
end-of-line marker consisting of either a CARRIAGE RETURN and a LINE FEED or
just a LINE FEED, and not by a CARRIAGE RETURN alone."
-}
streamEndOfLineP :: Get ()
streamEndOfLineP = word8 asciiLF <|> (word8 asciiCR >> word8 asciiLF)

whiteSpaceP :: Get ()
whiteSpaceP = skipMany $ satisfy isWhiteSpace

{-|
Get the contents of a stream.

From PDF specifications:

"The sequence of bytes that make up a stream lie between the end-of-line marker
following the stream keyword and the endstream keyword; the stream dictionary
specifies the exact number of bytes. There should be an end-of-line marker
after the data and before endstream; this marker shall not be included in the
stream length."
-}
streamP :: Maybe Int -> Get ByteString
streamP mCount = do
  string "stream"
  streamEndOfLineP

  case mCount of
    Nothing -> do
      bytes <- manyTill (satisfy (const True))
                        (string "endstream")
      whiteSpaceP
      return $! BS.pack bytes
    Just count -> do
      stream <- getByteString count
      whiteSpaceP
      string "endstream"
      whiteSpaceP
      return $! stream

{-|
Parse a `PDFXRefStream`, a `PDFObjectStream`, a `PDFIndirectObjectWithStream`
or a `PDFIndirectObject`.

From the PDF specifications:

"There shall not be any extra bytes, other than white-space, between endstream
and endobj."
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

  mStream <- case maybeQuery (getValue "Length") object of
    Nothing -> return Nothing
    Just (PDFNumber count) ->
      option Nothing (Just <$> streamP (Just (round count)))
    _anyOtherCase -> option Nothing (Just <$> streamP Nothing)

  string "endobj"

  case mStream of
    Nothing -> return $ PDFIndirectObject objectNumber revisionNumber object
    (Just stream) -> case (maybeQuery (getValue "Type") object, object) of
      (Just (PDFName "XRef"), PDFDictionary dict) -> return $ updateStream
        (PDFXRefStream objectNumber revisionNumber dict "")
        stream
      (Just (PDFName "ObjStm"), PDFDictionary dict) -> return $ updateStream
        (PDFObjectStream objectNumber revisionNumber dict "")
        stream
      (_, PDFDictionary dict) -> return $ updateStream
        (PDFIndirectObjectWithStream objectNumber revisionNumber dict "")
        stream
      _anyOtherCase -> fail "indirectObject"
