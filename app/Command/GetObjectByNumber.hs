module Command.GetObjectByNumber
  ( getObjectByNumber
  ) where

import Control.Monad.Trans.Class (MonadTrans (lift))

import Data.ByteString qualified as BS
import Data.Fallible (FallibleT)
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.PDF.PDFDocument (PDFDocument)
import Data.PDF.PDFObject
    ( PDFObject (PDFArray, PDFBool, PDFComment, PDFDictionary, PDFEndOfFile, PDFHexString, PDFIndirectObject, PDFIndirectObjectWithGraphics, PDFIndirectObjectWithStream, PDFKeyword, PDFName, PDFNull, PDFNumber, PDFObjectStream, PDFReference, PDFStartXRef, PDFString, PDFTrailer, PDFVersion, PDFXRef, PDFXRefStream)
    )
import Data.PDF.PDFWork (evalPDFWork, getObject)

import PDF.Object.Object.FromPDFObject (fromPDFObject)
import PDF.Processing.PDFWork (importObjects)

import Util.Number (fromInt, fromNumber)

type Level :: Type
data Level = Level !Int !Bool

inc :: Level -> Level
inc (Level level _display) = Level (level + 1) True

hide :: Level -> Level
hide (Level level _display) = Level level False

disp :: Level -> Level
disp (Level level _display) = Level level True

(%>) :: Level -> BS.ByteString -> BS.ByteString
(%>) (Level _level False) bytestring = bytestring
(%>) (Level level True) bytestring = BS.replicate level 32
                                  <> BS.replicate level 32
                                  <> bytestring

pretty :: Level -> PDFObject -> BS.ByteString
pretty level (PDFComment comment) = level %> "%" <> comment <> "\n"
pretty level (PDFVersion version) = level %> "PDF-" <> version <> "\n"
pretty level PDFEndOfFile = level %> "%%EOF\n"
pretty level (PDFNumber number) = level %> fromNumber number <> "\n"
pretty level keyword@(PDFKeyword _keyword) =
  level %> fromPDFObject keyword <> "\n"
pretty level name@(PDFName _name) = level %> fromPDFObject name <> "\n"
pretty level pdfString@(PDFString _pdfString) =
  level %> fromPDFObject pdfString <> "\n"
pretty level hexString@(PDFHexString _hexString) =
  level %> fromPDFObject hexString <> ">\n"
pretty level reference@(PDFReference _major _minor) =
  level %> fromPDFObject reference <> "\n"
pretty level (PDFArray array) =
     level
  %> "[\n" <> BS.concat (pretty (inc level) <$> toList array)
  <> disp level %> "]\n"
pretty level (PDFDictionary dict) =
     level
  %> "<<\n"
  <> BS.concat (map (\(key, value) -> inc level
                                   %> "/"
                                   <> key
                                   <> " "
                                   <> pretty (hide (inc level)) value)
                                   (Map.toList dict))
  <> disp level
  %> ">>\n"
pretty level (PDFIndirectObject major minor object) =
     level
  %> fromInt major
  <> " "
  <> fromInt minor
  <> " obj\n"
  <> pretty (inc level) object
  <> level
  %> "endobj\n"
pretty level (PDFIndirectObjectWithStream major minor dict _stream) =
     level
  %> fromInt major
  <> " "
  <> fromInt minor
  <> " obj\n"
  <> pretty (inc level) (PDFDictionary dict)
  <> level
  %> "stream ... endstream\n"
  <> level
  %> "endobj\n"
pretty level (PDFIndirectObjectWithGraphics major minor dict _gfx) =
     level
  %> fromInt major
  <> " "
  <> fromInt minor
  <> " obj\n"
  <> pretty (inc level) (PDFDictionary dict)
  <> level
  %> "stream ... endstream\n"
  <> level
  %> "endobj\n"
pretty level (PDFObjectStream major minor dict _stream) =
     level
  %> fromInt major
  <> " "
  <> fromInt minor
  <> " obj\n"
  <> pretty (inc level) (PDFDictionary dict)
  <> level
  %> "stream ... endstream\n"
  <> level
  %> "endobj\n"
pretty level (PDFXRefStream major minor dict _stream) =
     level
  %> fromInt major
  <> " "
  <> fromInt minor
  <> " obj\n"
  <> pretty (inc level) (PDFDictionary dict)
  <> level
  %> "stream ... endstream\n"
  <> level
  %> "endobj\n"
pretty level (PDFBool True) = level %> "true\n"
pretty level (PDFBool False) = level %> "false\n"
pretty level PDFNull = level %> "null\n"
pretty level (PDFXRef _subsections) = level %> "xref\n" <> "...\n"
pretty level (PDFTrailer trailer) =
  level %> "trailer\n" <> pretty (inc level) trailer
pretty level (PDFStartXRef start) =
  level %> "startxref\n" <> fromInt start <> "\n"

getObjectByNumber :: Int -> PDFDocument -> FallibleT IO ()
getObjectByNumber objectNumber objects = do

  evalPDFWork (importObjects objects >> getObject objectNumber) >>= \case
    Just object -> lift $ BS.putStr (pretty (Level 0 True) object)
    Nothing -> lift $ putStrLn "Object not found"
