module Pdf.Object.Object.FromPDFObject
  ( fromPDFObject
  ) where

import Data.Array (Array)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as BSU
import Data.Foldable (toList)
import Data.Map qualified as Map

import Pdf.Graphics.Object (separateGfx)
import Pdf.Graphics.Objects (Objects)
import Pdf.Object.Object.Delimiter (spaceIfNeeded)
import Pdf.Object.Object.FromXRef (fromXRef)
import Pdf.Object.Object.PDFObject
    ( PDFObject (PDFArray, PDFBool, PDFComment, PDFDictionary, PDFEndOfFile, PDFHexString, PDFIndirectObject, PDFIndirectObjectWithGraphics, PDFIndirectObjectWithStream, PDFKeyword, PDFName, PDFNull, PDFNumber, PDFObjectStream, PDFReference, PDFStartXRef, PDFString, PDFTrailer, PDFVersion, PDFXRef, PDFXRefStream)
    , mkEmptyPDFDictionary
    )

import Util.Dictionary (Dictionary)
import Util.Name (fromName)
import Util.Number (fromInt, fromNumber)
import Util.String (fromHexString, fromString)

{- |
Takes a `List` of `PDFObject`, converts them to the `ByteString` representation
and inserts spaces between them if necessary.
-}
separateObjects :: [PDFObject] -> BS.ByteString
separateObjects []                           = ""
separateObjects [object1                   ] = fromPDFObject object1
separateObjects (object1 : object2 : others) = BS.concat
  [ fromPDFObject object1
  , spaceIfNeeded object1 object2
  , separateObjects (object2 : others)
  ]

{- |
Converts a `PDFObject` to a `ByteString` ready to be inserted in an output
PDF file.
-}
fromPDFObject :: PDFObject -> BS.ByteString
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

fromArray :: Array PDFObject -> BS.ByteString
fromArray items = BS.concat ["[", separateObjects (toList items), "]"]

fromDictionary :: Dictionary PDFObject -> BS.ByteString
fromDictionary keyValues = BS.concat
  ["<<", separateObjects (splitCouple (Map.toList keyValues)), ">>"]
 where
  splitCouple [] = []
  splitCouple ((key, value) : remains) =
    PDFName key : value : splitCouple remains

fromIndirectObject :: Int -> Int -> PDFObject -> BS.ByteString
fromIndirectObject number revision object = BS.concat
  [ BSU.fromString (show number)
  , " "
  , BSU.fromString (show revision)
  , " obj\n"
  , fromPDFObject object
  , "\nendobj\n"
  ]

fromIndirectObjectWithStream
  :: Int -> Int -> Dictionary PDFObject -> BS.ByteString -> BS.ByteString
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

fromIndirectObjectWithGraphics
  :: Int -> Int -> Dictionary PDFObject -> Objects -> BS.ByteString
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
