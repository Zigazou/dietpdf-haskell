{-|
Pretty-prints a PDF indirect object by number, following references and
rendering a human-readable representation to standard output.

This module provides utilities to render 'PDFObject' values with indentation
control ('Level'), avoid infinite recursion via a processed set, and an entry
point 'getObjectByNumber' that fetches and prints the requested object.
-}
module Command.GetObjectByNumber
  ( getObjectByNumber
  ) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State.Lazy (StateT, evalStateT, gets, modify)

import Data.ByteString (ByteString)
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
import Data.PDF.WorkData (WorkData)
import Data.Set (Set)
import Data.Set qualified as Set

import PDF.Object.Object.FromPDFObject (fromPDFObject)
import PDF.Processing.PDFWork (importObjects)

import Util.Number (fromInt, fromNumber)

{-|
Indentation control for pretty-printing.

Carries the current indentation level (in steps of spaces) and a display flag
indicating whether indentation should be applied for the current element.
-}
type Level :: Type
data Level = Level !Int !Bool

{-|
Increase indentation level and enable display.
-}
inc :: Level -> Level
inc (Level level _display) = Level (level + 1) True

{-|
Increase indentation level while preserving the display flag.
-}
inc' :: Level -> Level
inc' (Level level display) = Level (level + 1) display

{-|
Keep the same indentation level but disable display (no leading spaces).
-}
hide :: Level -> Level
hide (Level level _display) = Level level False

{-|
Keep the same indentation level and enable display (apply leading spaces).
-}
disp :: Level -> Level
disp (Level level _display) = Level level True

{-|
Apply indentation to a 'ByteString' according to 'Level'.

If display is disabled, returns the input unchanged. If enabled, prefixes
spaces based on the current level.
-}
infixl 9 %>
(%>) :: Level -> ByteString -> ByteString
(%>) (Level _level False) bytestring = bytestring
(%>) (Level level True) bytestring = BS.replicate level 32
                                  <> BS.replicate level 32
                                  <> bytestring

{-|
Set of already visited indirect object numbers to avoid infinite recursion
when following references.
-}
type Processed :: Type
type Processed = Set Int

{-|
Rendering monad: the set of visited objects is shared globally across all
branches, so each indirect object is printed at most once.
-}
type Pretty :: (Type -> Type) -> Type -> Type
type Pretty m a = StateT Processed (StateT WorkData (FallibleT m)) a

{-|
Run the action only if object number is not yet visited, marking it visited.
-}
once :: Monad m => Int -> Pretty m ByteString -> Pretty m ByteString
once major action = do
  seen <- gets (Set.member major)
  if seen
    then return ""
    else modify (Set.insert major) >> action

{-|
Render a 'PDFObject' to a pretty 'ByteString' with indentation and recursion
control.

Takes the set of processed object numbers, the current 'Level', and the object
to render. Uses 'PDFWork' to fetch referenced objects as needed.
-}
pretty :: Monad m => Level -> PDFObject -> Pretty m ByteString
pretty level (PDFComment comment) =
  return $ level %> "%" <> comment <> "\n"

pretty level (PDFVersion version) =
  return $ level %> "PDF-" <> version <> "\n"

pretty level PDFEndOfFile =
  return $ level %> "%%EOF\n"

pretty level (PDFNumber number) =
  return $ level %> fromNumber number <> "\n"

pretty level keyword@(PDFKeyword _keyword) =
  return $ level %> fromPDFObject keyword <> "\n"

pretty level name@(PDFName _name) =
  return $ level %> fromPDFObject name <> "\n"

pretty level pdfString@(PDFString _pdfString) =
  return $ level %> fromPDFObject pdfString <> "\n"

pretty level hexString@(PDFHexString _hexString) =
  return $ level %> fromPDFObject hexString <> ">\n"

pretty level reference@(PDFReference major _minor) = do
  seen <- gets (Set.member major)
  if seen
    then return $ level %> fromPDFObject reference <> "\n"
    else lift (getObject major) >>= \case
      Just referenced -> pretty (inc' level) referenced
      Nothing         -> return ""

pretty level (PDFArray array) = do
  children <- mapM (pretty (disp (inc level))) (toList array)
  return $ level %> "[\n" <> BS.concat children <> disp level %> "]\n"

pretty level (PDFDictionary dict) = do
  children <- mapM
    ( \(key, value) -> do
        pKey <- pretty (inc level) (PDFName key)
        pValue <- if key == "Parent"
                    then return $ hide level %> fromPDFObject value <> "\n"
                    else pretty (hide (inc level)) value
        return $ BS.dropEnd 1 pKey <> " " <> pValue
    )
    (Map.toAscList dict)
  return $ level %> "<<\n" <> BS.concat children <> disp level %> ">>\n"

pretty level (PDFIndirectObject major minor object) = once major $ do
      child <- pretty (inc level) object
      return $
        level %> fromInt major <> " " <> fromInt minor <> " obj\n"
              <> child
              <> disp level %> "endobj\n"

pretty level (PDFIndirectObjectWithStream major minor dict _stream) = once major $ do
      child <- pretty (inc level) (PDFDictionary dict)
      return $
        level %> fromInt major <> " " <> fromInt minor <> " obj\n"
              <> child
              <> disp level %> "stream ... endstream\n"
              <> disp level %> "endobj\n"

pretty level (PDFIndirectObjectWithGraphics major minor dict _gfx) = once major $ do
      child <- pretty (inc level) (PDFDictionary dict)
      return $
        level %> fromInt major <> " " <> fromInt minor <> " obj\n"
              <> child
              <> disp level %> "stream ... endstream\n"
              <> disp level %> "endobj\n"

pretty level (PDFObjectStream major minor dict _stream) = once major $ do
      child <- pretty (inc level) (PDFDictionary dict)
      return $
        level %> fromInt major <> " " <> fromInt minor <> " obj\n"
              <> child
              <> level %> "stream ... endstream\n"
              <> level %> "endobj\n"

pretty level (PDFXRefStream major minor dict _stream) = once major $ do
      child <- pretty (inc level) (PDFDictionary dict)
      return $
        level %> fromInt major <> " " <> fromInt minor <> " obj\n"
              <> child
              <> level %> "stream ... endstream\n"
              <> level %> "endobj\n"

pretty level (PDFBool True) =
  return $ level %> "true\n"

pretty level (PDFBool False) =
  return $ level %> "false\n"

pretty level PDFNull =
  return $ level %> "null\n"

pretty level (PDFXRef _subsections) =
  return $ level %> "xref\n" <> "...\n"

pretty level (PDFTrailer trailer) = do
  pTrailer <- pretty (inc level) trailer
  return $ level %> "trailer\n" <> pTrailer

pretty level (PDFStartXRef start) =
  return $ level %> "startxref\n" <> fromInt start <> "\n"

{-|
Fetch an indirect object by number and pretty-print it.

Imports the given 'PDFDocument' into the current 'PDFWork' context, then
renders the object using 'pretty'. If the object cannot be found, returns a
diagnostic message.
-}
printObject :: Logging m => Int -> PDFDocument -> PDFWork m ByteString
printObject objectNumber objects = do
  importObjects objects
  getObject objectNumber >>= \case
    Just object -> evalStateT (pretty (Level 0 True) object) Set.empty
    Nothing -> return "Object not found"

{-|
Entry point: pretty-print the object with the given number to standard output.

Evaluates 'printObject' within 'PDFWork' and writes the resulting
'ByteString' via 'lift'/'BS.putStr'.
-}
getObjectByNumber :: Int -> PDFDocument -> FallibleT IO ()
getObjectByNumber objectNumber objects =
  evalPDFWork (printObject objectNumber objects) >>= lift . BS.putStr
