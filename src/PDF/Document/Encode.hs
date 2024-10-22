module PDF.Document.Encode
  ( -- * Encoding
    pdfEncode
    -- * XRef generation
  , calcOffsets
  , encodeObject
  ) where

import Control.Monad (when, (>=>))
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets)

import Data.ByteString qualified as BS
import Data.Context (Contextual (ctx))
import Data.IntMap qualified as IM
import Data.Logging (Logging)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.PDF.EncodedObject (EncodedObject (EncodedObject), eoBinaryData)
import Data.PDF.PDFDocument (PDFDocument, fromList)
import Data.PDF.PDFObject
    ( PDFObject (PDFDictionary, PDFEndOfFile, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNull, PDFObjectStream, PDFReference, PDFStartXRef, PDFTrailer, PDFVersion)
    , getObjectNumber
    )
import Data.PDF.PDFPartition
    ( PDFPartition (ppObjectsWithStream, ppObjectsWithoutStream)
    )
import Data.PDF.PDFWork
    ( PDFWork
    , getTrailer
    , hasNoVersion
    , isEmptyPDF
    , lastObjectNumber
    , modifyIndirectObjects
    , pushContext
    , putNewObject
    , sayP
    , setTrailer
    , setTranslationTable
    , throwError
    , withStreamCount
    , withoutStreamCount, putObject
    )
import Data.PDF.Settings
    ( OptimizeGFX (DoNotOptimizeGFX, OptimizeGFX)
    , Settings (sOptimizeGFX)
    )
import Data.PDF.WorkData (WorkData (wPDF, wSettings))
import Data.Sequence qualified as SQ
import Data.Text qualified as T
import Data.TranslationTable (getTranslationTable)
import Data.UnifiedError
    ( UnifiedError (EncodeEncrypted, EncodeNoIndirectObject, EncodeNoTrailer, EncodeNoVersion)
    )

import GHC.IO.Handle (BufferMode (LineBuffering))

import PDF.Document.MergeVectorStream (mergeVectorStream)
import PDF.Document.ObjectStream (explodeList, makeObjectStreamFromObjects)
import PDF.Document.XRef (calcOffsets, xrefStreamTable)
import PDF.Object.Object.FromPDFObject (fromPDFObject)
import PDF.Object.Object.Properties (getValueForKey, hasKey)
import PDF.Object.Object.RenameResources (renameResources)
import PDF.Object.State (getValue, setMaybe)
import PDF.Processing.Optimize (optimize)
import PDF.Processing.PDFWork
    ( clean
    , importObjects
    , pMapP
    , pModifyIndirectObjects
    )

import System.IO (hSetBuffering, stderr)

import Util.ByteString (toNameBase)
import Util.Sequence (mapMaybe)

{- |
Encodes a PDF object and keeps track of its number and length.

Returns an `EncodedObject` which contains the object's number, the length of its
byte representation, the byte data, and any embedded objects.
-}
encodeObject :: Logging m => PDFObject -> PDFWork m EncodedObject
encodeObject object@(PDFIndirectObject number _ _) = return $
    EncodedObject number (BS.length bytes) bytes SQ.Empty
  where bytes = fromPDFObject object
encodeObject object@(PDFIndirectObjectWithStream number _ _ _) = return $
    EncodedObject number (BS.length bytes) bytes SQ.Empty
  where bytes = fromPDFObject object
encodeObject object@(PDFObjectStream number _ _ _) = do
  let bytes = fromPDFObject object
  embeddedObjects <- explodeList [object]
  return $ EncodedObject
            number
            (BS.length bytes)
            bytes
            (mapMaybe getObjectNumber (SQ.fromList embeddedObjects))

encodeObject object = return $ EncodedObject 0 (BS.length bytes) bytes SQ.Empty
  where bytes = fromPDFObject object

{- |
Updates an XRef stream object by copying certain fields ("Root", "Info", "ID")
from a given trailer object.

Returns the updated XRef stream object.
-}
updateXRefStm :: Logging m => PDFObject -> PDFObject -> PDFWork m PDFObject
updateXRefStm trailer xRefStm = do
  mRoot <- getValue "Root" trailer
  mInfo <- getValue "Info" trailer
  mID   <- getValue "ID" trailer

  setMaybe "Root" mRoot xRefStm
    >>= setMaybe "Info" mInfo
    >>= setMaybe "ID" mID

{- |
Finds all resource names in a collection of PDF objects.

Resources are typically stored in a dictionary object with a "Resources" key.
-}
getAllResourceNames :: Monad m => PDFWork m [BS.ByteString]
getAllResourceNames =
  gets (concat . getAllResourceNames' . ppObjectsWithoutStream . wPDF)
 where
  getAllResourceNames' :: IM.IntMap PDFObject -> [[BS.ByteString]]
  getAllResourceNames' objects = do
    (_, object) <- IM.toList objects
    (return . concat . catMaybes)
      [ getResourceKeys "ColorSpace" object
      , getResourceKeys "Font"       object
      , getResourceKeys "XObject"    object
      , getResourceKeys "ExtGState"  object
      , getResourceKeys "Properties" object
      , getResourceKeys "Pattern"    object
      , getKeys         "ColorSpace" object
      , getKeys         "Font"       object
      , getKeys         "XObject"    object
      , getKeys         "ExtGState"  object
      , getKeys         "Properties" object
      , getKeys         "Pattern"    object
      ]

  getKeys :: BS.ByteString -> PDFObject -> Maybe [BS.ByteString]
  getKeys key object = do
    getValueForKey key object >>= \case
      PDFDictionary dict -> Just $ Map.keys dict
      _notADictionary    -> Nothing

  getResourceKeys :: BS.ByteString -> PDFObject -> Maybe [BS.ByteString]
  getResourceKeys key object = do
    resources <- getValueForKey "Resources" object
    getValueForKey key resources >>= \case
      PDFDictionary dict -> Just $ Map.keys dict
      _notADictionary    -> Nothing

mergePagesContents :: Logging m => PDFObject -> PDFWork m PDFObject
mergePagesContents object@(PDFIndirectObject major minor (PDFDictionary dict)) = do
  let mType     = getValueForKey "Type"     object
      mContents = getValueForKey "Contents" object

  case (mType, mContents) of
    (Just (PDFName "Page"), Just vectors) -> do
      streamNumber <- mergeVectorStream vectors >>= putNewObject
      let newDict = Map.insert "Contents" (PDFReference streamNumber 0) dict
      return $ PDFIndirectObject major minor (PDFDictionary newDict)

    _anyOtherObject -> return object

mergePagesContents object = return object

{- |
Given a list of PDF objects, generate the PDF file content.

This function recreates the XRef table in the old format.

An error is signaled in the following cases:

- no numbered objects in the list of PDF objects
- no PDF version in the list of PDF objects
- no trailer in the list of PDF objects
-}
pdfEncode
  :: PDFDocument -- ^ A collection of PDF objects (order matters)
  -> PDFWork IO BS.ByteString -- ^ A unified error or a bytestring
pdfEncode objects = do
  _ <- liftIO $ hSetBuffering stderr LineBuffering

  importObjects objects
  whenM isEmptyPDF (throwError EncodeNoIndirectObject)
  whenM hasNoVersion (throwError EncodeNoVersion)

  pushContext $ ctx ("encode" :: String)

  wsCount <- withStreamCount
  wosCount <- withoutStreamCount

  sayP $ T.concat [ "Indirect object with stream: ", T.pack (show wsCount) ]
  sayP $ T.concat [ "Indirect object without stream: ", T.pack (show wosCount) ]

  pdfTrailer <- getTrailer

  when (pdfTrailer == PDFTrailer PDFNull) (throwError EncodeNoTrailer)
  when (hasKey "Encrypt" pdfTrailer) (throwError EncodeEncrypted)

  setTrailer pdfTrailer

  sayP "Merging pages contents"
  gets (ppObjectsWithoutStream . wPDF)
    >>= mapM_ (mergePagesContents >=> putObject)

  clean

  resourceNames <- getAllResourceNames

  -- Do not create a translation table if GFX won't be optimized.
  optGFX <- gets (sOptimizeGFX . wSettings)
  let nameTranslations = case optGFX of
        OptimizeGFX      -> getTranslationTable toNameBase resourceNames
        DoNotOptimizeGFX -> Map.empty

  setTranslationTable nameTranslations

  modifyIndirectObjects (renameResources nameTranslations)

  sayP $ T.concat [ "Found "
                  , T.pack . show $ Map.size nameTranslations
                  , " resource names"
                  ]

  sayP "Optimizing PDF"
  pModifyIndirectObjects optimize

  clean

  nextObjectNumber <- (+ 1) <$> lastObjectNumber

  sayP "Grouping objects without stream"
  objectsWithoutStream <- gets ( fromList
                               . fmap snd
                               . IM.toList
                               . ppObjectsWithoutStream
                               . wPDF
                               )

  objectStream <- makeObjectStreamFromObjects objectsWithoutStream
                                              nextObjectNumber

  sayP "Encoding PDF"
  encodedObjStm  <- optimize objectStream >>= encodeObject
  encodedStreams <- gets (ppObjectsWithStream . wPDF) >>= pMapP encodeObject

  let
    encodedAll = IM.insert nextObjectNumber encodedObjStm encodedStreams
    body = BS.concat $ eoBinaryData . snd <$> IM.toAscList encodedAll

  let pdfHead = fromPDFObject (PDFVersion "1.7")
      pdfEnd  = fromPDFObject PDFEndOfFile

  sayP "Optimize XRef stream table"
  xref <- do
    let
      xrefst = xrefStreamTable (nextObjectNumber + 1)
                               (BS.length pdfHead)
                               encodedAll

    optimize xrefst >>= updateXRefStm pdfTrailer

  let
    encodedXRef = fromPDFObject xref
    xRefStmOffset = BS.length pdfHead + BS.length body
    startxref = fromPDFObject (PDFStartXRef xRefStmOffset)

  sayP "PDF has been optimized!"

  return $ BS.concat [pdfHead, body, encodedXRef, startxref, pdfEnd]
