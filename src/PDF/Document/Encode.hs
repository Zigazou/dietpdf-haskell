{- |
Encode a 'PDFDocument' into a PDF file bytestring.

This module contains the high-level PDF encoder used by the application. It
imports a parsed 'Data.PDF.PDFDocument.PDFDocument' into the working state,
applies a series of cleanups and optimizations, and finally serializes the
result into a strict 'ByteString'.

In addition to encoding individual objects, the encoder can generate the cross
reference information needed by the PDF reader (either as an XRef stream object,
or by computing offsets for the classic xref table format).
-}
module PDF.Document.Encode
  ( pdfEncode
  , calcOffsets
  , encodeObject
  ) where

import Control.Monad (when, (>=>))
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Context (Contextual (ctx))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Logging (Logging)
import Data.Map.Strict qualified as Map
import Data.PDF.EncodedObject (EncodedObject (EncodedObject), eoBinaryData)
import Data.PDF.PDFDocument (PDFDocument, fromList)
import Data.PDF.PDFObject
  ( PDFObject (PDFArray, PDFDictionary, PDFEndOfFile, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNull, PDFNumber, PDFObjectStream, PDFReference, PDFStartXRef, PDFTrailer, PDFVersion)
  , getObjectNumber
  )
import Data.PDF.PDFObjects (toPDFDocument)
import Data.PDF.PDFPartition
  (PDFPartition (ppObjectsWithStream, ppObjectsWithoutStream))
import Data.PDF.PDFWork
  ( PDFWork
  , getTrailer
  , getTranslationTable
  , hasNoVersion
  , isEmptyPDF
  , lastObjectNumber
  , modifyIndirectObjects
  , modifyIndirectObjectsP
  , pushContext
  , putNewObject
  , putObject
  , sayP
  , setMasks
  , setTrailer
  , throwError
  , withStreamCount
  , withoutStreamCount
  )
import Data.PDF.WorkData (WorkData (wPDF))
import Data.Set qualified as Set
import Data.Sequence qualified as SQ
import Data.Text qualified as T
import Data.UnifiedError
  ( UnifiedError (EncodeEncrypted, EncodeNoIndirectObject, EncodeNoTrailer, EncodeNoVersion)
  )

import GHC.IO.Handle (BufferMode (LineBuffering))

import PDF.Document.OptimizeBitmapMasks (optimizeBitmapMasks)
import PDF.Document.GetAllMasks (getAllMasks)
import PDF.Document.InvisibleImages (removeInvisiblePageImages)
import PDF.Document.MergeVectorStream (mergeVectorStream)
import PDF.Document.ObjectStream (explodeList, makeObjectStreamFromObjects)
import PDF.Document.OptimizeNumbers (optimizeNumbers)
import PDF.Document.OptimizeOptionalDictionaryEntries
  (optimizeOptionalDictionaryEntries)
import PDF.Document.OptimizeResources (optimizeResources)
import PDF.Document.ResourceContext (buildStreamResources)
import PDF.Document.Resources
  (removeUnusedResources, updateWithAdditionalResources)
import PDF.Document.XRef (calcOffsets, xrefStreamTable)
import PDF.Document.ZeroFillMaskedImages (zeroFillMaskedImages)
import PDF.Object.Object.FromPDFObject (fromPDFObject)
import PDF.Object.Object.Properties (getValueForKey, hasKey, isCatalog)
import PDF.Object.State (getValue, setMaybe)
import PDF.Processing.DuplicatedObjects
  (convertDuplicatedReferences, duplicateCount, findDuplicatedObjects)
import PDF.Processing.Optimize (optimize)
import PDF.Processing.PDFWork (importObjects, pMapP, removeUnusedObjects)
import PDF.Processing.RepeatedFormFragments (repeatedFormFragments)

import System.IO (hSetBuffering, stderr)

import Util.Dictionary (mkDictionary, Dictionary)
import Util.Sequence (mapMaybe)

-- Removing a form can expose further unused resources. Repeat until neither
-- resource dictionaries nor the reachable object graph changes.
pruneUnusedResources :: PDFWork IO ()
pruneUnusedResources = do
  before <- gets wPDF
  removeUnusedResources
  removeUnusedObjects
  after <- gets wPDF
  when (after /= before) pruneUnusedResources

{- |
Encodes a PDF object and keeps track of its number and length.

Returns an `EncodedObject` which contains the object's number, the length of its
byte representation, the byte data, and any embedded objects.
-}
encodeObject :: Logging m => PDFObject -> PDFWork m EncodedObject
encodeObject object@(PDFIndirectObject number _ _) = return $
    EncodedObject number (BS.length bytes) bytes SQ.Empty
  where
    bytes :: ByteString
    bytes = fromPDFObject object

encodeObject object@(PDFIndirectObjectWithStream number _ _ _) = return $
    EncodedObject number (BS.length bytes) bytes SQ.Empty
  where
    bytes :: ByteString
    bytes = fromPDFObject object

encodeObject object@(PDFObjectStream number _ _ _) = do
  let
    bytes :: ByteString
    bytes = fromPDFObject object

  embeddedObjects <- explodeList [object]

  return $ EncodedObject
            number
            (BS.length bytes)
            bytes
            (mapMaybe getObjectNumber (SQ.fromList embeddedObjects))

encodeObject object =
  return $ EncodedObject 0 (BS.length bytes) bytes SQ.Empty
 where
  bytes :: ByteString
  bytes = fromPDFObject object

{-|
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

{-|
Checks whether a PDF object declares the @BrotliDecode@ filter, either as a
lone filter name or within a filter array.
-}
usesBrotliFilter :: PDFObject -> Bool
usesBrotliFilter object = case getValueForKey "Filter" object of
  Just (PDFName "BrotliDecode") -> True
  Just (PDFArray filters)       -> PDFName "BrotliDecode" `elem` filters
  _anyOtherValue                -> False

{-|
Declares the PDF Association's Brotli extension (Brotli RFC 7932, published as
an extension to PDF 2.0: BaseVersion 2.0, ExtensionLevel 1, ExtensionRevision
2026) on the document catalog, so that conforming readers can recognize
@/BrotliDecode@ streams even when the file header is later downgraded.
-}
declareBrotliExtension :: PDFObject -> PDFObject
declareBrotliExtension (PDFIndirectObject major minor (PDFDictionary dict)) =
  PDFIndirectObject major
                    minor
                    (PDFDictionary (Map.insert "Extensions" extensions dict))
 where
  extensions :: PDFObject
  extensions = PDFDictionary $ mkDictionary
    [ ( "PDFA"
      , PDFDictionary $ mkDictionary
          [ ("BaseVersion", PDFName "2.0")
          , ("ExtensionLevel", PDFNumber 1)
          , ("ExtensionRevision", PDFNumber 2026)
          ]
      )
    ]

declareBrotliExtension object = object

{-|
Merge the contents streams of all pages into a single stream.
-}
mergePagesContents :: Logging m => PDFObject -> PDFWork m PDFObject
mergePagesContents object@(PDFIndirectObject major minor (PDFDictionary dict)) = do
  let
    mType :: Maybe PDFObject
    mType = getValueForKey "Type" object

    mContents :: Maybe PDFObject
    mContents = getValueForKey "Contents" object

  case (mType, mContents) of
    (Just (PDFName "Page"), Just vectors) -> do
      streamNumber <- mergeVectorStream vectors
        >>= removeInvisiblePageImages object
        >>= putNewObject

      let
        newDict :: Map.Map ByteString PDFObject
        newDict = Map.insert "Contents" (PDFReference streamNumber 0) dict

      return $ PDFIndirectObject major minor (PDFDictionary newDict)

    _anyOtherObject -> return object

mergePagesContents object = return object

{-|
Encode a 'PDFDocument' into a PDF file.

This function imports the provided document into the working state, performs a
number of normalization and optimization passes, and finally serializes the
result.

The encoder writes cross-reference information using an XRef stream object.

An error is signaled in the following cases:

- no numbered objects in the list of PDF objects
- no PDF version in the list of PDF objects
- no trailer in the list of PDF objects
-}
pdfEncode
  :: PDFDocument
  -> PDFWork IO ByteString
pdfEncode objects = do
  liftIO $ hSetBuffering stderr LineBuffering

  -- Import objects and validate prerequisites.
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

  -- Remove duplicate objects and assigns references accordingly.
  duplicated <- gets wPDF >>= findDuplicatedObjects

  let
    dupCount :: Int
    dupCount = duplicateCount duplicated

  if dupCount == 0
    then
      sayP "No duplicated objects found"
    else do
      sayP $ T.concat [ "Cleaning duplicate objects ("
                      , T.pack (show (duplicateCount duplicated))
                      , " duplicate(s) found)"
                      ]

      convertDuplicatedReferences duplicated
      removeUnusedObjects

  -- Merge page contents streams.
  sayP "Merging pages contents"
  gets (ppObjectsWithoutStream . wPDF)
    >>= mapM_ (mergePagesContents >=> putObject)

  sayP "Pruning unused resources"
  pruneUnusedResources

  -- Optimize numbers and resources.
  sayP "Optimizing numbers"
  optimizeNumbers

  sayP "Optimizing resources"
  optimizeResources

  -- Find all masks (masks supports more "destruction" than standard images).
  sayP "Finding all masks"
  wosMasks <- gets (getAllMasks . toPDFDocument . ppObjectsWithoutStream . wPDF)
  wsMasks <- gets (getAllMasks . toPDFDocument . ppObjectsWithStream . wPDF)
  setMasks (wosMasks <> wsMasks)

  -- Optimize optional dictionary entries (entries which can be omitted).
  sayP "Optimizing optional dictionary entries"
  modifyIndirectObjects optimizeOptionalDictionaryEntries

  nameTranslations <- getTranslationTable
  sayP $ T.concat [ "Found "
                  , T.pack . show $ Map.size nameTranslations
                  , " resource names"
                  ]

  -- Zero-fill image pixels hidden by a soft mask before the generic filter
  -- search below re-optimizes the resulting stream.
  sayP "Zero-filling masked images"
  modifyIndirectObjectsP zeroFillMaskedImages

  sayP "Optimizing bitmap masks"
  bitmapMasks <- optimizeBitmapMasks

  sayP "Optimizing PDF"
  -- Snapshot resource ownership after resource renaming and before parallel
  -- work.
  streamResources <- gets (buildStreamResources . (\pdf ->
    ppObjectsWithoutStream pdf <> ppObjectsWithStream pdf) . wPDF)
  modifyIndirectObjectsP $ \object ->
    let
      resources :: Maybe (Dictionary PDFObject)
      resources = getObjectNumber object >>= \number ->
          IM.findWithDefault Nothing number streamResources
    in
      if maybe False (`Set.member` bitmapMasks) (getObjectNumber object)
        then return object
        else optimize resources object

  updateWithAdditionalResources
  repeatedFormFragments
  pruneUnusedResources

  -- Brotli (BrotliDecode) is a PDF 2.0 extension: bump the header version and
  -- declare it in the catalog's Extensions dictionary when used.
  usesBrotli <- gets (any usesBrotliFilter . ppObjectsWithStream . wPDF)
  when usesBrotli $ do
    sayP "BrotliDecode filter found: forcing PDF version to 2.0"
    modifyIndirectObjects (\object -> if isCatalog object
                                        then declareBrotliExtension object
                                        else object
                           )

  nextObjectNumber <- (+ 1) <$> lastObjectNumber

  -- Group objects without stream into an object stream.
  sayP "Grouping objects without stream"
  objectsWithoutStream <- gets ( fromList
                               . fmap snd
                               . IM.toList
                               . ppObjectsWithoutStream
                               . wPDF
                               )

  sayP "Making object stream from objects"
  objectStream <- makeObjectStreamFromObjects objectsWithoutStream
                                              nextObjectNumber

  -- Encode all objects.
  sayP "Encoding PDF"
  encodedObjStm  <- optimize Nothing objectStream >>= encodeObject
  encodedStreams <- gets (ppObjectsWithStream . wPDF) >>= pMapP encodeObject

  let
    encodedAll :: IntMap EncodedObject
    encodedAll = IM.insert nextObjectNumber encodedObjStm encodedStreams

    body :: ByteString
    body = BS.concat $ eoBinaryData . snd <$> IM.toAscList encodedAll

  let
    pdfVersion :: PDFObject
    pdfVersion = PDFVersion (if usesBrotli then "2.0" else "1.7")

    pdfHead :: ByteString
    pdfHead = fromPDFObject pdfVersion

    pdfEnd  :: ByteString
    pdfEnd  = fromPDFObject PDFEndOfFile

  -- Generate the XRef table.
  sayP "Optimizing XRef stream table"
  xref <- do
    let
      xrefst :: PDFObject
      xrefst = xrefStreamTable (nextObjectNumber + 1)
                               (BS.length pdfHead)
                               encodedAll

    optimize Nothing xrefst >>= updateXRefStm pdfTrailer

  let
    encodedXRef :: ByteString
    encodedXRef = fromPDFObject xref

    xRefStmOffset :: Int
    xRefStmOffset = BS.length pdfHead + BS.length body

    startxref :: ByteString
    startxref = fromPDFObject (PDFStartXRef xRefStmOffset)

  sayP "PDF has been optimized!"

  -- Return the final PDF bytestring.
  return $ BS.concat [pdfHead, body, encodedXRef, startxref, pdfEnd]
