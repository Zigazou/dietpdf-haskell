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
                                                  , PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFObjectStream
                                                  , PDFStartXRef
                                                  , PDFReference
                                                  )
                                                , fromPDFObject
                                                , xrefCount
                                                , hasKey
                                                )
import           Pdf.Document.Document          ( PDFDocument
                                                , dFilter
                                                , findLast
                                                , deepFind
                                                , lMap
                                                , fromList
                                                )
import           Pdf.Document.Partition         ( PDFPartition
                                                  ( ppHeads
                                                  , ppIndirectObjects
                                                  , ppTrailers
                                                  )
                                                , firstVersion
                                                , lastTrailer
                                                , toPartition
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
                                                )
import           Pdf.Document.Collection        ( encodeObject
                                                , eoBinaryData
                                                )
import           Pdf.Document.Uncompress        ( uncompress )
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

removeUnused :: Logging m => PDFDocument -> FallibleT m PDFDocument
removeUnused doc = do
  references <- deepFind isReference <$> uncompress doc
  return $ dFilter (used references) doc
 where
  isNotLinearized :: PDFObject -> Bool
  isNotLinearized = not . hasKey "Linearized"

  isReferenced :: PDFDocument -> PDFObject -> Bool
  isReferenced refs (PDFIndirectObject num gen _) =
    PDFReference num gen `elem` refs
  isReferenced refs (PDFIndirectObjectWithStream num gen _ _) =
    PDFReference num gen `elem` refs
  isReferenced refs (PDFObjectStream num gen _ _) =
    PDFReference num gen `elem` refs
  isReferenced _anyRefs _anyOtherObject = True

  used :: PDFDocument -> PDFObject -> Bool
  used refs object = isNotLinearized object && isReferenced refs object

  isReference :: PDFObject -> Bool
  isReference PDFReference{}  = True
  isReference _anyOtherObject = False

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
pdfEncode document = do
  objects <- tryF (removeUnused document) >>= \case
    Right unusedRemoved -> do
      sayF "Unable to remove unused objects"
      return unusedRemoved
    Left _anyError -> do
      sayF "Unused objects removed"
      return document

  let partObjects = foldr ((<>) . toPartition) mempty objects
      pdfTrailer  = lastTrailer partObjects

      pdfHead     = (fromPDFObject . firstVersion) partObjects
      pdfEnd      = fromPDFObject PDFEndOfFile

  when (null $ ppIndirectObjects partObjects) $ do
    sayF "Indirect objects not found"
    throwE EncodeNoIndirectObject

  when (null $ ppHeads partObjects) $ do
    sayF "Version not found"
    throwE EncodeNoVersion

  when (null $ ppTrailers partObjects) $ do
    sayF "Trailer not found"
    throwE EncodeNoTrailer

  sayF "Optimizing PDF"

  optimizeds <- sequence (lMap optimize (ppIndirectObjects partObjects))

  sayF "Encoding PDF"
  let
    encodeds    = encodeObject <$> optimizeds
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
