module Pdf.Document.Encode
  ( -- * Encoding
    pdfEncode
    -- * XRef generation
  , calcOffsets
  , encodeObject
  ) where

import Control.Monad (when)
import Control.Monad.Trans.Except (runExcept, throwE)

import Data.ByteString qualified as BS
import Data.IntMap qualified as IM
import Data.Map.Strict qualified as Map
import Util.Sequence (mapMaybe)
import Data.Text qualified as T

import Pdf.Document.Collection (findLast, fromPDFDocument)
import Pdf.Document.Document (PDFDocument, cFilter, fromList, singleton)
import Pdf.Document.EncodedObject (EncodedObject (EncodedObject), eoBinaryData)
import Pdf.Document.ObjectStream (explodeDocument, explodeList, insert)
import Pdf.Document.Partition
    ( PDFPartition (PDFPartition, ppHeads, ppObjectsWithStream, ppObjectsWithoutStream, ppTrailers)
    , lastTrailer
    , removeUnused
    )
import Pdf.Document.XRef (calcOffsets, xrefStreamTable)
import Pdf.Object.Object.FromPDFObject (fromPDFObject)
import Pdf.Object.Object.PDFObject
    ( PDFObject (PDFDictionary, PDFEndOfFile, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNull, PDFObjectStream, PDFReference, PDFStartXRef, PDFTrailer, PDFVersion, PDFXRefStream)
    )
import Pdf.Object.Object.Properties
    ( getObjectNumber
    , hasKey
    , hasStream
    , isHeader
    , isIndirect
    , isTrailer
    )
import Pdf.Object.Optimize (optimize)
import Pdf.Object.State (getValue, setMaybe)

import Util.Dictionary (mkDictionary)
import Util.Logging (Logging, sayComparisonF, sayErrorF, sayF)
import Util.UnifiedError
    ( FallibleT
    , UnifiedError (EncodeNoIndirectObject, EncodeNoTrailer, EncodeNoVersion)
    , tryF
    )
import Data.Sequence qualified as SQ

-- | Encodes a PDF object and keep track of its number and length.
encodeObject :: Logging m => PDFObject -> FallibleT m EncodedObject
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

updateXRefStm :: Logging m => PDFObject -> PDFObject -> FallibleT m PDFObject
updateXRefStm trailer xRefStm = do
  mRoot <- getValue "Root" trailer
  mInfo <- getValue "Info" trailer
  mID <- getValue "ID" trailer
  setMaybe "Root" mRoot xRefStm >>= setMaybe "Info" mInfo >>= setMaybe "ID" mID

isCatalog :: PDFObject -> Bool
isCatalog object@PDFIndirectObject{} =
  case runExcept (getValue "Type" object) of
    Left  _     -> False
    Right value -> value == Just (PDFName "Catalog")
isCatalog _anyOtherObject = False

isInfo :: PDFObject -> Bool
isInfo object@PDFIndirectObject{} = hasKey "Author" object
isInfo _anyOtherObject            = False

getTrailer :: PDFPartition -> PDFObject
getTrailer partition = case lastTrailer partition of
  (PDFTrailer PDFNull) ->
    let
      catalog = findLast isCatalog (ppObjectsWithoutStream partition)
      info    = findLast isInfo (ppObjectsWithoutStream partition)
    in
      case (catalog, info) of
        ( Just (PDFIndirectObject cNumber cRevision _),
          Just (PDFIndirectObject iNumber iRevision _))
          -> PDFTrailer
            (PDFDictionary $ mkDictionary
              [ ("Root", PDFReference cNumber cRevision)
              , ("Info", PDFReference iNumber iRevision)
              ]
            )
        (Just (PDFIndirectObject cNumber cRevision _), Nothing) -> PDFTrailer
          ( PDFDictionary
          $ mkDictionary [("Root", PDFReference cNumber cRevision)]
          )
        _anyOtherCase -> PDFTrailer PDFNull
  (PDFXRefStream _ _ dict _) ->
    let catalog = Map.lookup "Root" dict
        info    = Map.lookup "Info" dict
    in  case (catalog, info) of
          (Just rCatalog, Just rInfo) ->
            PDFTrailer
              ( PDFDictionary
              $ mkDictionary [("Root", rCatalog), ("Info", rInfo)]
              )
          _anyOtherCase -> PDFTrailer PDFNull
  validTrailer -> validTrailer

objectToEmbed :: PDFObject -> Bool
objectToEmbed object = isIndirect object
                    && (not . hasStream) object
                    -- && (not . isInfo) object

{-|
Given a list of PDF objects, generate the PDF file content.

This function recreates the XRef table in the old format.

An error is signaled in the following cases:

- no numbered objects in the list of PDF objects
- no PDF version in the list of PDF objects
- no trailer in the list of PDF objects
-}
{-# INLINE pdfEncode #-}
pdfEncode
  :: Logging m
  => PDFDocument -- ^ A collection of PDF objects (order matters)
  -> FallibleT m BS.ByteString -- ^ A unified error or a bytestring
pdfEncode objects = do
  -- Extract objects embedded in object streams
  allObjects <- explodeDocument objects

  let partition = PDFPartition
        { ppObjectsWithStream    = fromPDFDocument $ cFilter hasStream allObjects
        , ppObjectsWithoutStream = fromPDFDocument $ cFilter objectToEmbed allObjects
        , ppHeads                = cFilter isHeader allObjects
        , ppTrailers             = cFilter isTrailer allObjects
        }

      pdfHead = fromPDFObject (PDFVersion "1.7")
      pdfEnd  = fromPDFObject PDFEndOfFile

  sayF "Starting discovery"

  when (null (ppObjectsWithStream partition) && null (ppObjectsWithoutStream partition)) $ do
    sayF "Indirect objects not found"
    throwE EncodeNoIndirectObject

  let withStreamCount = IM.size (ppObjectsWithStream partition)
      withoutStreamCount = IM.size (ppObjectsWithoutStream partition)

  sayF $ T.concat ["Indirect object with stream: ", T.pack (show withStreamCount)]
  sayF $ T.concat ["Indirect object without stream: ", T.pack (show withoutStreamCount)]

  when (null $ ppHeads partition) $ do
    sayF "Version not found"
    throwE EncodeNoVersion

  sayF "Version found"

  let pdfTrailer = getTrailer partition

  when (pdfTrailer == PDFTrailer PDFNull) $ do
    sayF "Trailer not found"
    throwE EncodeNoTrailer

  sayF "Trailer found"

  sayF "Optimizing PDF"

  oObjectsWithStream <- mapM optimize (ppObjectsWithStream partition)
  oObjectsWithoutStream <- mapM optimize (ppObjectsWithoutStream partition)
  let oPartition = partition { ppObjectsWithStream    = oObjectsWithStream
                             , ppObjectsWithoutStream = oObjectsWithoutStream
                             , ppTrailers             = singleton pdfTrailer
                             }

  sayF "Last cleaning"
  cleaned <- tryF (removeUnused oPartition) >>= \case
    Right unusedRemoved -> do
      let allCount = IM.size (ppObjectsWithStream partition)
                   + IM.size (ppObjectsWithoutStream partition)
          usedCount = IM.size (ppObjectsWithStream unusedRemoved)
                    + IM.size (ppObjectsWithoutStream unusedRemoved)
      sayComparisonF "Unused objects removal" allCount usedCount
      return unusedRemoved
    Left theError -> do
      sayErrorF "Unable to remove unused objects" theError
      return oPartition

  sayF "Calculating last object number"
  let
    lastObjectNumber = max (fst . IM.findMax $ ppObjectsWithStream cleaned)
                           (fst . IM.findMax $ ppObjectsWithoutStream cleaned)

  sayF "Grouping objects without stream"
  let objectsWithoutStream = fromList $ snd <$> IM.toList (ppObjectsWithoutStream cleaned)
  objectStream <- insert objectsWithoutStream (lastObjectNumber + 1)

  sayF "Encoding PDF"
  encodedObjStm <- optimize objectStream >>= encodeObject
  encodedStreams <- mapM encodeObject (ppObjectsWithStream cleaned)
  let
    encodedAll = IM.insert (lastObjectNumber + 1) encodedObjStm encodedStreams
    body = BS.concat $ eoBinaryData . snd <$> IM.toAscList encodedAll

  sayF "Optimize XRef stream table"
  xref <- do
    let
      xrefst = xrefStreamTable (lastObjectNumber + 2)
                               (BS.length pdfHead)
                               encodedAll

    optimize xrefst >>= updateXRefStm pdfTrailer

  let
    encodedXRef = fromPDFObject xref
    xRefStmOffset = BS.length pdfHead + BS.length body
    startxref = fromPDFObject (PDFStartXRef xRefStmOffset)

  sayF "PDF has been optimized!"

  return $ BS.concat [pdfHead, body, encodedXRef, startxref, pdfEnd]
