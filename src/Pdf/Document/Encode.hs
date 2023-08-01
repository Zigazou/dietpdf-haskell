{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE LambdaCase #-}
module Pdf.Document.Encode
  ( -- * Encoding
    pdfEncode
    -- * XRef generation
  , calcOffsets
  , encodeObject
  ) where

import qualified Data.ByteString               as BS
import qualified Data.Map.Strict               as Map
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFEndOfFile
                                                  , PDFNumber
                                                  , PDFStartXRef
                                                  , PDFTrailer
                                                  , PDFNull
                                                  , PDFIndirectObject
                                                  , PDFDictionary
                                                  , PDFReference
                                                  , PDFName
                                                  , PDFXRefStream
                                                  )
                                                , fromPDFObject
                                                , xrefCount
                                                , hasKey
                                                , isIndirect
                                                , isHeader
                                                , isTrailer
                                                )
import           Pdf.Document.Document          ( PDFDocument
                                                , cFilter
                                                , findLast
                                                , fromList
                                                , toList
                                                , cfMap
                                                , cSize
                                                , singleton
                                                )
import           Pdf.Document.Partition         ( PDFPartition
                                                  ( ppHeads
                                                  , ppIndirectObjects
                                                  , ppTrailers
                                                  , PDFPartition
                                                  )
                                                , firstVersion
                                                , lastTrailer
                                                , removeUnused
                                                )
import           Util.UnifiedError              ( UnifiedError
                                                  ( EncodeNoIndirectObject
                                                  , EncodeNoVersion
                                                  , EncodeNoTrailer
                                                  )
                                                , FallibleT
                                                , tryF
                                                )
import           Pdf.Object.Optimize            ( optimize )
import           Pdf.Document.XRef              ( calcOffsets
                                                , xrefTable
                                                )
import           Util.Logging                   ( Logging
                                                , sayF
                                                , sayComparisonF
                                                , sayErrorF
                                                )
import           Pdf.Document.Collection        ( encodeObject
                                                , eoBinaryData
                                                )
import           Pdf.Object.State               ( setValue
                                                , getValue
                                                )
import           Control.Monad.Trans.Except     ( throwE
                                                , runExcept
                                                )
import           Control.Monad                  ( when )
import           Pdf.Document.ObjectStream      ( explode )
import           Util.Dictionary                ( mkDictionary )

updateTrailer :: Logging m => Int -> PDFObject -> FallibleT m PDFObject
updateTrailer entriesCount =
  setValue "Size" (PDFNumber $ fromIntegral entriesCount)

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
pdfEncode
  :: Logging m
  => PDFDocument -- ^ A collection of PDF objects (order matters)
  -> FallibleT m BS.ByteString -- ^ A unified error or a bytestring
pdfEncode objects = do
  -- Extract objects embedded in object streams
  exploded <- explode objects

  let partition = PDFPartition
        { ppIndirectObjects = cFilter isIndirect exploded
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

  oIndirectObjects <- cfMap optimize (ppIndirectObjects partition)
  let oPartition = partition { ppIndirectObjects = oIndirectObjects
                             , ppTrailers        = singleton pdfTrailer
                             }

  sayF "Last cleaning"
  cleaned <- tryF (removeUnused oPartition) >>= \case
    Right unusedRemoved -> do
      sayComparisonF "Unused objects removal"
                     (cSize (ppIndirectObjects partition))
                     (cSize (ppIndirectObjects unusedRemoved))
      return unusedRemoved
    Left theError -> do
      sayErrorF "Unable to remove unused objects" theError
      return oPartition

  sayF "Encoding PDF"
  let
    encodeds    = encodeObject <$> toList (ppIndirectObjects cleaned)
    body        = BS.concat $ eoBinaryData <$> encodeds
    xref        = xrefTable (BS.length pdfHead) (fromList encodeds)
    encodedXRef = fromPDFObject xref
    startxref =
      fromPDFObject (PDFStartXRef (BS.length pdfHead + BS.length body))

  trailer <- fromPDFObject <$> updateTrailer (xrefCount xref) pdfTrailer

  return $ BS.concat [pdfHead, body, encodedXRef, trailer, startxref, pdfEnd]
