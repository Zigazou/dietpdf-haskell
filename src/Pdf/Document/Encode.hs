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
                                                )
import           Pdf.Document.Collection        ( encodeObject
                                                , eoBinaryData
                                                )
import           Pdf.Object.State               ( setValue
                                                , getDictionary
                                                )
import           Control.Monad.Trans.Except     ( throwE )
import           Control.Monad                  ( when )

updateTrailer
  :: Logging m => PDFObject -> Int -> PDFObject -> FallibleT m PDFObject
updateTrailer root entriesCount object = do
  dict <- getDictionary object
  setValue "Size" (PDFNumber $ fromIntegral entriesCount) object
    >>= setValue "Root" (Map.findWithDefault root "Root" dict)

findRoot :: PDFDocument -> Maybe PDFObject
findRoot = findLast (hasKey "Root")

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
  let partition = PDFPartition { ppIndirectObjects = cFilter isIndirect objects
                               , ppHeads           = cFilter isHeader objects
                               , ppTrailers        = cFilter isTrailer objects
                               }
      pdfTrailer = lastTrailer partition

      pdfHead    = (fromPDFObject . firstVersion) partition
      pdfEnd     = fromPDFObject PDFEndOfFile

  sayF "Starting discovery"

  when (null $ ppIndirectObjects partition) $ do
    sayF "Indirect objects not found"
    throwE EncodeNoIndirectObject

  sayF "Indirect objects found"

  when (null $ ppHeads partition) $ do
    sayF "Version not found"
    throwE EncodeNoVersion

  sayF "Version found"

  when (null $ ppTrailers partition) $ do
    sayF "Trailer not found"
    throwE EncodeNoTrailer

  sayF "Trailer found"

  sayF "Optimizing PDF"

  oIndirectObjects <- cfMap optimize (ppIndirectObjects partition)
  let oPartition = partition { ppIndirectObjects = oIndirectObjects }

  sayF "Last cleaning"
  cleaned <- tryF (removeUnused oPartition) >>= \case
    Right unusedRemoved -> do
      sayComparisonF "Unused objects removal"
                     (cSize (ppIndirectObjects partition))
                     (cSize (ppIndirectObjects unusedRemoved))
      return unusedRemoved
    Left _anyError -> do
      sayF "  - Unable to remove unused objects"
      return oPartition

  sayF "Encoding PDF"
  let
    encodeds    = encodeObject <$> toList (ppIndirectObjects cleaned)
    body        = BS.concat $ eoBinaryData <$> encodeds
    xref        = xrefTable (BS.length pdfHead) (fromList encodeds)
    encodedXRef = fromPDFObject xref
    startxref =
      fromPDFObject (PDFStartXRef (BS.length pdfHead + BS.length body))

  trailer <-
    fromPDFObject
      <$> updateTrailer
            (case findRoot objects of
              Nothing   -> PDFNumber 0
              Just root -> root
            )
            (xrefCount xref)
            pdfTrailer

  return $ BS.concat [pdfHead, body, encodedXRef, trailer, startxref, pdfEnd]
