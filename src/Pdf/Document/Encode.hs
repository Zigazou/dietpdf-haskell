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

import Pdf.Document.Collection
    ( encodeObject
    , eoBinaryData
    , findLast
    , fromPDFDocument
    )
import Pdf.Document.Document (PDFDocument, cFilter, singleton)
import Pdf.Document.ObjectStream (explodeDocument)
import Pdf.Document.Partition
    ( PDFPartition (PDFPartition, ppHeads, ppIndirectObjects, ppTrailers)
    , firstVersion
    , lastTrailer
    , removeUnused
    )
import Pdf.Document.XRef (calcOffsets, xrefStreamTable)
import Pdf.Object.Object
    ( PDFObject (PDFDictionary, PDFEndOfFile, PDFIndirectObject, PDFName, PDFNull, PDFNumber, PDFReference, PDFStartXRef, PDFTrailer, PDFXRefStream)
    , fromPDFObject
    , hasKey
    , isHeader
    , isIndirect
    , isTrailer
    , xrefCount
    )
import Pdf.Object.Optimize (optimize)
import Pdf.Object.State (getValue, setMaybe, setValue)

import Util.Dictionary (mkDictionary)
import Util.Logging (Logging, sayComparisonF, sayErrorF, sayF)
import Util.UnifiedError
    ( FallibleT
    , UnifiedError (EncodeNoIndirectObject, EncodeNoTrailer, EncodeNoVersion)
    , tryF
    )

updateXRefStm :: Logging m => PDFObject -> PDFObject -> FallibleT m PDFObject
updateXRefStm xRefStm trailer = do
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
      catalog = findLast isCatalog (ppIndirectObjects partition)
      info    = findLast isInfo (ppIndirectObjects partition)
    in
      case (catalog, info) of
        (Just (PDFIndirectObject cNumber cRevision _), Just (PDFIndirectObject iNumber iRevision _))
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
  exploded <- explodeDocument objects

  let partition = PDFPartition
        { ppIndirectObjects = fromPDFDocument $ cFilter isIndirect exploded
        , ppHeads           = cFilter isHeader exploded
        , ppTrailers        = cFilter isTrailer exploded
        }

      pdfHead = (fromPDFObject . firstVersion) partition
      pdfEnd  = fromPDFObject PDFEndOfFile

  sayF "Starting discovery"

  when (null $ ppIndirectObjects partition) $ do
    sayF "Indirect objects not found"
    throwE EncodeNoIndirectObject

  sayF "Indirect objects found"

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

  oIndirectObjects <- mapM optimize (ppIndirectObjects partition)
  let oPartition = partition { ppIndirectObjects = oIndirectObjects
                             , ppTrailers        = singleton pdfTrailer
                             }

  sayF "Last cleaning"
  cleaned <- tryF (removeUnused oPartition) >>= \case
    Right unusedRemoved -> do
      sayComparisonF "Unused objects removal"
                     (IM.size (ppIndirectObjects partition))
                     (IM.size (ppIndirectObjects unusedRemoved))
      return unusedRemoved
    Left theError -> do
      sayErrorF "Unable to remove unused objects" theError
      return oPartition

  sayF "Encoding PDF"
  let
    encodeds    = encodeObject <$> ppIndirectObjects cleaned
    body        = BS.concat $ eoBinaryData . snd <$> IM.toAscList encodeds
    xrefObjectNumber = fst . IM.findMax $ encodeds

  sayF "Optimize XRef stream table"
  xref <- (optimize $ xrefStreamTable (xrefObjectNumber + 1) (BS.length pdfHead) encodeds) >>= updateXRefStm pdfTrailer

  let
    encodedXRef = fromPDFObject xref
    xRefStmOffset = BS.length pdfHead + BS.length body
    startxref = fromPDFObject (PDFStartXRef xRefStmOffset)

  sayF "PDF has been optimized!"

  return $ BS.concat [pdfHead, body, encodedXRef, startxref, pdfEnd]
