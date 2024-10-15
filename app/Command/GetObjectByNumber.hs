module Command.GetObjectByNumber
  ( getObjectByNumber
  ) where

import Control.Monad.Trans.Class (MonadTrans (lift))

import Data.ByteString qualified as BS
import Data.Fallible (FallibleT)
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.Logging (Logging)
import Data.Map qualified as Map
import Data.PDF.PDFDocument (PDFDocument)
import Data.PDF.PDFObject
    ( PDFObject (PDFArray, PDFBool, PDFComment, PDFDictionary, PDFEndOfFile, PDFHexString, PDFIndirectObject, PDFIndirectObjectWithGraphics, PDFIndirectObjectWithStream, PDFKeyword, PDFName, PDFNull, PDFNumber, PDFObjectStream, PDFReference, PDFStartXRef, PDFString, PDFTrailer, PDFVersion, PDFXRef, PDFXRefStream)
    )
import Data.PDF.PDFWork (PDFWork, evalPDFWork, getObject)
import Data.Set (Set)
import Data.Set qualified as Set

import PDF.Object.Object.FromPDFObject (fromPDFObject)
import PDF.Processing.PDFWork (importObjects)

import Util.Number (fromInt, fromNumber)

type Level :: Type
data Level = Level !Int !Bool

inc :: Level -> Level
inc (Level level _display) = Level (level + 1) True

inc' :: Level -> Level
inc' (Level level display) = Level (level + 1) display

hide :: Level -> Level
hide (Level level _display) = Level level False

disp :: Level -> Level
disp (Level level _display) = Level level True

infixl 9 %>
(%>) :: Level -> BS.ByteString -> BS.ByteString
(%>) (Level _level False) bytestring = bytestring
(%>) (Level level True) bytestring = BS.replicate level 32
                                  <> BS.replicate level 32
                                  <> bytestring

type Processed :: Type
type Processed = Set Int

pretty :: Monad m => Processed -> Level -> PDFObject -> PDFWork m BS.ByteString
pretty _processed level (PDFComment comment) =
  return $ level %> "%" <> comment <> "\n"

pretty _processed level (PDFVersion version) =
  return $ level %> "PDF-" <> version <> "\n"

pretty _processed level PDFEndOfFile =
  return $ level %> "%%EOF\n"

pretty _processed level (PDFNumber number) =
  return $ level %> fromNumber number <> "\n"

pretty _processed level keyword@(PDFKeyword _keyword) =
  return $ level %> fromPDFObject keyword <> "\n"

pretty _processed level name@(PDFName _name) =
  return $ level %> fromPDFObject name <> "\n"

pretty _processed level pdfString@(PDFString _pdfString) =
  return $ level %> fromPDFObject pdfString <> "\n"

pretty _processed level hexString@(PDFHexString _hexString) =
  return $ level %> fromPDFObject hexString <> ">\n"

pretty processed level reference@(PDFReference major _minor)
  | Set.member major processed = return $
      level %> fromPDFObject reference <> "\n"
  | otherwise = getObject major >>= \case
      Just referenced -> pretty processed (inc' level) referenced
      Nothing         -> return ""

pretty processed level (PDFArray array) = do
  children <- mapM (pretty processed (disp (inc level))) (toList array)
  return $ level %> "[\n" <> BS.concat children <> disp level %> "]\n"

pretty processed level (PDFDictionary dict) = do
  children <- mapM
    ( \(key, value) -> do
        pKey <- pretty processed (inc level) (PDFName key)
        pValue <- if key == "Parent"
                    then return $ hide level %> fromPDFObject value <> "\n"
                    else pretty processed (hide (inc level)) value
        return $ BS.dropEnd 1 pKey <> " " <> pValue
    )
    (Map.toList dict)
  return $ level %> "<<\n" <> BS.concat children <> disp level %> ">>\n"

pretty processed level (PDFIndirectObject major minor object)
  | Set.member major processed = return ""
  | otherwise = do
      let processed' = Set.insert major processed
      child <- pretty processed' (inc level) object
      return $
        level %> fromInt major <> " " <> fromInt minor <> " obj\n"
              <> child
              <> disp level %> "endobj\n"

pretty processed level (PDFIndirectObjectWithStream major minor dict _stream)
  | Set.member major processed = return ""
  | otherwise = do
      let processed' = Set.insert major processed
      child <- pretty processed' (inc level) (PDFDictionary dict)
      return $
        level %> fromInt major <> " " <> fromInt minor <> " obj\n"
              <> child
              <> disp level %> "stream ... endstream\n"
              <> disp level %> "endobj\n"

pretty processed level (PDFIndirectObjectWithGraphics major minor dict _gfx)
  | Set.member major processed = return ""
  | otherwise = do
      let processed' = Set.insert major processed
      child <- pretty processed' (inc level) (PDFDictionary dict)
      return $
        level %> fromInt major <> " " <> fromInt minor <> " obj\n"
              <> child
              <> disp level %> "stream ... endstream\n"
              <> disp level %> "endobj\n"

pretty processed level (PDFObjectStream major minor dict _stream)
  | Set.member major processed = return ""
  | otherwise = do
      let processed' = Set.insert major processed
      child <- pretty processed' (inc level) (PDFDictionary dict)
      return $
        level %> fromInt major <> " " <> fromInt minor <> " obj\n"
              <> child
              <> level %> "stream ... endstream\n"
              <> level %> "endobj\n"

pretty processed level (PDFXRefStream major minor dict _stream)
  | Set.member major processed = return ""
  | otherwise = do
      let processed' = Set.insert major processed
      child <- pretty processed' (inc level) (PDFDictionary dict)
      return $
        level %> fromInt major <> " " <> fromInt minor <> " obj\n"
              <> child
              <> level %> "stream ... endstream\n"
              <> level %> "endobj\n"

pretty _processed level (PDFBool True) =
  return $ level %> "true\n"

pretty _processed level (PDFBool False) =
  return $ level %> "false\n"

pretty _processed level PDFNull =
  return $ level %> "null\n"

pretty _processed level (PDFXRef _subsections) =
  return $ level %> "xref\n" <> "...\n"

pretty processed level (PDFTrailer trailer) = do
  pTrailer <- pretty processed (inc level) trailer
  return $ level %> "trailer\n" <> pTrailer

pretty _processed level (PDFStartXRef start) =
  return $ level %> "startxref\n" <> fromInt start <> "\n"

printObject :: Logging m => Int -> PDFDocument -> PDFWork m BS.ByteString
printObject objectNumber objects = do
  importObjects objects
  getObject objectNumber >>= \case
    Just object -> pretty Set.empty (Level 0 True) object
    Nothing -> return "Object not found"

getObjectByNumber :: Int -> PDFDocument -> FallibleT IO ()
getObjectByNumber objectNumber objects =
  evalPDFWork (printObject objectNumber objects) >>= lift . BS.putStr
