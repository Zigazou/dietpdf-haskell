{-|
Parser for PDF indirect objects

This module provides a binary parser for PDF indirect objects.

Any object in a PDF file may be labelled as an indirect object. This gives the
object a unique object identifier by which other objects can refer to it.

The object identifier consists of two parts:

- A positive integer object number. Indirect objects may be numbered
  sequentially within a PDF file, but this is not required; object numbers may
  be assigned in any arbitrary order.
- A non-negative integer generation number. In a newly created file, all
  indirect objects have generation numbers of 0. Nonzero generation numbers may
  be introduced when the file is later updated.

Together, the combination of an object number and a generation number uniquely
identify an indirect object.

The definition of an indirect object in a PDF file consists of its object number
and generation number (separated by white space), followed by the value of the
object bracketed between the keywords @obj@ and @endobj@.

Beginning with PDF 1.5, indirect objects may reside in object streams. They are
referred to in the same way; however, their definition does not include the
keywords @obj@ and @endobj@, and their generation number is zero.
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

{-|
Parse a single decimal digit (0-9).

Returns the byte value of the digit character.
-}
digit :: Get Word8
digit = satisfy isDigit

{-|
Parse a PDF object that may appear as a value in an indirect object.

An item can be any of the following PDF object types:

- name object
- string object (literal string)
- reference to an indirect object
- number (integer or real)
- keyword
- hexadecimal string
- array
- dictionary

The parser tries each item type in sequence using alternative operators,
returning the first successful parse.
-}
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
Parse one or more consecutive decimal digits as a list of byte values.

The parser requires at least one digit and returns all consecutive digits as a
list of their byte values (before numeric conversion).
-}
integerP :: Get [Word8]
integerP = some' digit

{-|
Parse the end-of-line marker that follows the @stream@ keyword.

According to PDF specifications, the @stream@ keyword must be followed by an
end-of-line marker consisting of either:

- A line feed (LF, 0x0A) alone, or
- A carriage return (CR, 0x0D) followed by a line feed (LF)

A carriage return (CR) alone is not a valid stream end-of-line marker.
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
specifies the exact number of bytes. There should be an end-of-line marker after
the data and before endstream; this marker shall not be included in the stream
length."
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
Parse an indirect PDF object in a PDF document.

An indirect object consists of:

1. Object number (non-negative integer)
2. Generation number (non-negative integer)
3. The @obj@ keyword
4. An object value (any PDF object type)
5. Optional stream content (for stream objects)
6. The @endobj@ keyword

The function automatically detects stream objects by examining the @Length@
entry in the object's dictionary. For special stream types (XRefStream,
ObjectStream), the corresponding PDF object type is created.

According to PDF specifications, there must not be any extra bytes other than
whitespace between @endstream@ and @endobj@.

Returns:

- 'PDFIndirectObject' for regular indirect objects
- 'PDFIndirectObjectWithStream' for objects with stream content
- 'PDFXRefStream' for cross-reference stream objects
- 'PDFObjectStream' for object stream objects
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
