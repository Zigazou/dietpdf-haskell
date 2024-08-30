module Pdf.Object.Object.ObjectInfo
  ( objectInfo
  ) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text.Lazy qualified as TL

import Formatting (format, int, text, (%))
import Formatting.ByteStringFormatter (utf8)

import Pdf.Object.Object.PDFObject
    ( PDFObject (PDFArray, PDFBool, PDFComment, PDFDictionary, PDFEndOfFile, PDFHexString, PDFIndirectObject, PDFIndirectObjectWithGraphics, PDFIndirectObjectWithStream, PDFKeyword, PDFName, PDFNull, PDFNumber, PDFObjectStream, PDFReference, PDFStartXRef, PDFString, PDFTrailer, PDFVersion, PDFXRef, PDFXRefStream)
    )
import Pdf.Object.Object.Properties (objectType, xrefCount)

import Util.Number (fromNumber)

{- | Returns a `Text` string describing a PDFObject.
-}
objectInfo :: PDFObject -> TL.Text
objectInfo (PDFComment comment) = format ("{- " % utf8 % " -}") comment
objectInfo (PDFVersion version) = format ("VERSION=" % utf8) version
objectInfo PDFEndOfFile         = "END-OF-FILE"
objectInfo (PDFNumber number) =
  format ("[number:" % utf8 % "]") (fromNumber number)
objectInfo (PDFKeyword keyword) = format ("[keyword:" % utf8 % "]") keyword
objectInfo (PDFName    name   ) = format ("[name:" % utf8 % "]") name
objectInfo (PDFString  bytes  ) = format ("[string:" % utf8 % "]") bytes
objectInfo (PDFHexString hexstring) =
  format ("[hexstring: " % utf8 % "]") hexstring
objectInfo (PDFReference number revision) =
  format ("[ref:" % int % "," % int % "]") number revision
objectInfo (PDFArray objects) =
  format ("[array:count=" % int % "]") (length objects)
objectInfo (PDFDictionary dictionary) = case objectType dictionary of
  Just value -> format ("[dictionary:type=" % utf8 % ";count=" % int % "]")
                       value
                       (Map.size dictionary)
  Nothing -> format ("[dictionary:count=" % int % "]") (Map.size dictionary)
objectInfo (PDFIndirectObject number revision object) = format
  ("object(" % int % "," % int % ")=" % text)
  number
  revision
  (objectInfo object)
objectInfo (PDFIndirectObjectWithStream number revision dict stream) = format
  ("object(" % int % "," % int % ")=" % text % "+[stream:length=" % int % "]")
  number
  revision
  (objectInfo . PDFDictionary $ dict)
  (BS.length stream)
objectInfo (PDFIndirectObjectWithGraphics number revision dict _) = format
  ("graphics(" % int % "," % int % ")=" % text % "]")
  number
  revision
  (objectInfo . PDFDictionary $ dict)
objectInfo (PDFObjectStream number revision object stream) = format
  ( "objectstream("
  % int
  % ","
  % int
  % ")="
  % text
  % "+[stream:length="
  % int
  % "]"
  )
  number
  revision
  (objectInfo . PDFDictionary $ object)
  (BS.length stream)
objectInfo (PDFXRefStream number revision object stream) = format
  ( "xrefstream("
  % int
  % ","
  % int
  % ")="
  % text
  % "+[stream:length="
  % int
  % "]"
  )
  number
  revision
  (objectInfo . PDFDictionary $ object)
  (BS.length stream)
objectInfo (PDFBool True ) = "true"
objectInfo (PDFBool False) = "false"
objectInfo PDFNull         = "null"
objectInfo xref@(PDFXRef _) =
  format ("[xref:count=" % int % "]") (xrefCount xref)
objectInfo (PDFTrailer dictionary@(PDFDictionary _)) =
  format ("trailer(" % text % ")") (objectInfo dictionary)
objectInfo (PDFTrailer _) = "<trailer without dictionary>"
objectInfo (PDFStartXRef offset) =
  format ("[startxref:offset=" % int % "]") offset
