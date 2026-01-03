{-|
Conversion of PDF objects to bytestring representation

This module provides functions to convert internal PDF object representations
into their serialized bytestring form suitable for output to PDF files. It
handles proper spacing between objects to ensure valid PDF syntax.
-}
module PDF.Object.Object.FromPDFObject
  ( fromPDFObject
  ) where

import Data.Array (Array)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as BSU
import Data.Foldable (toList)
import Data.Map qualified as Map
import Data.PDF.GFXObject (separateGfx)
import Data.PDF.GFXObjects (GFXObjects)
import Data.PDF.PDFObject
    ( PDFObject (PDFArray, PDFBool, PDFComment, PDFDictionary, PDFEndOfFile, PDFHexString, PDFIndirectObject, PDFIndirectObjectWithGraphics, PDFIndirectObjectWithStream, PDFKeyword, PDFName, PDFNull, PDFNumber, PDFObjectStream, PDFReference, PDFStartXRef, PDFString, PDFTrailer, PDFVersion, PDFXRef, PDFXRefStream)
    , mkEmptyPDFDictionary
    )

import PDF.Object.Object.Delimiter (spaceIfNeeded)
import PDF.Object.Object.FromXRef (fromXRef)

import Util.Dictionary (Dictionary)
import Util.Name (fromName)
import Util.Number (fromInt, fromNumber)
import Util.String (fromHexString, fromString)

{-|
Serialize a list of PDF objects to a bytestring with appropriate spacing.

Converts each object in the list to its bytestring representation and inserts
whitespace delimiters between objects only where necessary. This ensures valid
PDF syntax without redundant whitespace.

Returns an empty bytestring for an empty list, or the concatenated serialized
objects with minimal spacing.
-}
separateObjects :: [PDFObject] -> ByteString
separateObjects []                           = ""
separateObjects [object1                   ] = fromPDFObject object1
separateObjects (object1 : object2 : others) = BS.concat
  [ fromPDFObject object1
  , spaceIfNeeded object1 object2
  , separateObjects (object2 : others)
  ]

{-|
Convert a PDF object to its bytestring representation.

This function handles all types of PDF objects and produces a bytestring
suitable for direct insertion into a PDF file. The conversion respects PDF
syntax requirements including:

- Comments with percent prefix and newline terminator
- Version header with PDF marker comments
- EOF marker
- Number and keyword formatting
- Name objects with solidus prefix
- String and hex string encoding
- Reference format (object number, revision, 'R' operator)
- Array and dictionary structures
- Indirect objects with optional streams or graphics
- Xref and trailer structures
- Boolean and null values

Returns the fully formatted bytestring representation.
-}
fromPDFObject :: PDFObject -> ByteString
fromPDFObject (PDFComment comment)     = BS.concat ["%", comment, "\n"]
fromPDFObject (PDFVersion version)     =
  BS.concat ["%PDF-", version, "\n%\xc0\xca\xc0\xda\n"]
fromPDFObject PDFEndOfFile             = "%%EOF\n"
fromPDFObject (PDFNumber    number   ) = fromNumber number
fromPDFObject (PDFKeyword   keyword  ) = keyword
fromPDFObject (PDFName      name     ) = fromName name
fromPDFObject (PDFString    bytes    ) = fromString bytes
fromPDFObject (PDFHexString hexstring) = fromHexString hexstring
fromPDFObject (PDFReference number revision) =
  BS.concat [fromInt number, " ", fromInt revision, " R"]
fromPDFObject (PDFArray      objects   ) = fromArray objects
fromPDFObject (PDFDictionary dictionary) = fromDictionary dictionary
fromPDFObject (PDFIndirectObject number revision object) =
  fromIndirectObject number revision object
fromPDFObject (PDFIndirectObjectWithStream number revision dict stream) =
  fromIndirectObjectWithStream number revision dict stream
fromPDFObject (PDFIndirectObjectWithGraphics number revision dict gfx) =
  fromIndirectObjectWithGraphics number revision dict gfx
fromPDFObject (PDFObjectStream number revision dict stream) =
  fromIndirectObjectWithStream number revision dict stream
fromPDFObject (PDFXRefStream number revision dict stream) =
  fromIndirectObjectWithStream number revision dict stream
fromPDFObject (PDFBool True ) = "true"
fromPDFObject (PDFBool False) = "false"
fromPDFObject PDFNull         = "null"
fromPDFObject (PDFXRef xrss)  = fromXRef xrss
fromPDFObject (PDFTrailer (PDFDictionary dictionary)) =
  BS.concat ["trailer\n", fromDictionary dictionary, "\n"]
fromPDFObject (PDFTrailer _) = error "a trailer can only contain a dictionary"
fromPDFObject (PDFStartXRef offset) =
  BS.concat ["startxref\n", fromInt offset, "\n"]

{-|
Convert a PDF array to its bytestring representation.

An array is serialized as square brackets containing all array elements with
appropriate spacing between them.

__Parameters:__

- An array of PDF objects

__Returns:__ A bytestring in the form @[item1 item2 ... itemN]@.
-}
fromArray :: Array PDFObject -> ByteString
fromArray items = BS.concat ["[", separateObjects (toList items), "]"]

{-|
Convert a PDF dictionary to its bytestring representation.

A dictionary is serialized as double angle brackets containing key-value pairs
in ascending order by key name. Each key is converted to a name object, and keys
and values are separated with appropriate spacing.

__Parameters:__

- A dictionary mapping name keys to PDF object values

__Returns:__ A bytestring in the form @<< /key1 value1 /key2 value2 ... >>@.
-}
fromDictionary :: Dictionary PDFObject -> ByteString
fromDictionary keyValues = BS.concat
  ["<<", separateObjects (splitCouple (Map.toAscList keyValues)), ">>"]
 where
  splitCouple [] = []
  splitCouple ((key, value) : remains) =
    PDFName key : value : splitCouple remains

{-|
Convert an indirect PDF object to its bytestring representation.

An indirect object is serialized with its object number, generation number, the
@obj@ keyword, the object content, and the @endobj@ terminator.

__Parameters:__

- Object number (non-negative integer)
- Generation number (revision number, typically 0)
- The PDF object to wrap

__Returns:__ A bytestring in the form @n m obj ... endobj@.
-}
fromIndirectObject :: Int -> Int -> PDFObject -> ByteString
fromIndirectObject number revision object = BS.concat
  [ BSU.fromString (show number)
  , " "
  , BSU.fromString (show revision)
  , " obj\n"
  , fromPDFObject object
  , "\nendobj\n"
  ]

{-|
Convert an indirect PDF object with an associated stream to its bytestring
representation.

An indirect object with a stream is serialized with:

1. Object number, generation number, and @obj@ keyword
2. A dictionary (typically containing stream metadata like length, filter, etc.)
3. The @stream@ keyword
4. Raw stream content (binary data)
5. The @endstream@ keyword
6. The @endobj@ terminator

__Parameters:__

- Object number (non-negative integer)
- Generation number (revision number, typically 0)
- Dictionary containing stream metadata
- Raw bytestring content of the stream

__Returns:__ A bytestring in the form @n m obj << ... >> stream ... endstream
endobj@.
-}
fromIndirectObjectWithStream
  :: Int -> Int -> Dictionary PDFObject -> ByteString -> ByteString
fromIndirectObjectWithStream number revision dict stream = BS.concat
  [ BSU.fromString (show number)
  , " "
  , BSU.fromString (show revision)
  , " obj\n"
  , fromDictionary dict
  , spaceIfNeeded mkEmptyPDFDictionary (PDFKeyword "stream")
  , "stream\n"
  , stream
  , "endstream"
  , "\nendobj\n"
  ]

{-|
Convert an indirect PDF object with graphics content to its bytestring
representation.

Similar to objects with streams, but the content is serialized from graphics
objects (GFX objects) rather than raw binary data. The graphics objects are
converted to their PDF operator notation before being written to the stream.

__Parameters:__

- Object number (non-negative integer)
- Generation number (revision number, typically 0)
- Dictionary containing stream metadata
- Graphics objects to serialize

__Returns:__ A bytestring in the form @n m obj << ... >> stream ... endstream
endobj@.
-}
fromIndirectObjectWithGraphics
  :: Int -> Int -> Dictionary PDFObject -> GFXObjects -> ByteString
fromIndirectObjectWithGraphics number revision dict gfx = BS.concat
  [ BSU.fromString (show number)
  , " "
  , BSU.fromString (show revision)
  , " obj\n"
  , fromDictionary dict
  , spaceIfNeeded mkEmptyPDFDictionary (PDFKeyword "stream")
  , "stream\n"
  , separateGfx gfx
  , "endstream"
  , "\nendobj\n"
  ]
