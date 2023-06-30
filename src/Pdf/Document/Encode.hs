{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
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
                                                  ( PDFDictionary
                                                  , PDFEndOfFile
                                                  , PDFNumber
                                                  , PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFObjectStream
                                                  , PDFStartXRef
                                                  , PDFTrailer
                                                  , PDFReference
                                                  )
                                                , fromPDFObject
                                                , xrefCount
                                                , hasKey
                                                )
<<<<<<< HEAD:src/Pdf/Document/Encode.hs
import           Pdf.Object.Partition           ( PDFPartition
=======
import           Pdf.Document.Document          ( PDFDocument
                                                , dFilter
                                                , findLast
                                                , cMap
                                                , deepFind
                                                )
import           Pdf.Document.Partition         ( PDFPartition
>>>>>>> 2cda7c92fcf859f588ef19db6e288dd3ad74727d:src/Pdf/Object/Encode.hs
                                                  ( ppHeads
                                                  , ppIndirectObjects
                                                  , ppTrailers
                                                  )
                                                , firstVersion
                                                , lastTrailer
                                                , toPartition
                                                )
import           Util.Errors                    ( UnifiedError
                                                  ( EncodeNoIndirectObject
                                                  , EncodeNoVersion
                                                  , EncodeNoTrailer
                                                  )
                                                )
import           Pdf.Object.Optimize            ( optimize )
import           Pdf.Document.XRef              ( calcOffsets
                                                , xrefTable
                                                )
<<<<<<< HEAD:src/Pdf/Document/Encode.hs
import           Data.Maybe                     ( isNothing )
import           Pdf.Object.Collection          ( findLastValue
                                                , encodeObject
                                                , eoBinaryData
                                                )
import           Util.Step                      ( StepM
                                                , StepT
                                                , stepT
                                                , throwError
                                                )
import           Control.Monad                  ( forM )
=======
import           Pdf.Document.Collection        ( encodeObject
                                                , eoBinaryData
                                                )
import           Pdf.Document.Uncompress        ( uncompress )
>>>>>>> 2cda7c92fcf859f588ef19db6e288dd3ad74727d:src/Pdf/Object/Encode.hs

updateTrailer :: PDFObject -> Int -> PDFObject -> PDFObject
updateTrailer root entriesCount (PDFTrailer (PDFDictionary dict)) = PDFTrailer
  (PDFDictionary
<<<<<<< HEAD:src/Pdf/Document/Encode.hs
    (HM.union
      (HM.fromList
        [ ("Size", PDFNumber (fromIntegral entriesCount))
        , ("Root", HM.lookupDefault root "Root" dict)
        ]
      )
      dict
=======
    (Map.fromList
      [ ("Size", PDFNumber (fromIntegral entriesCount))
      , ("Root", Map.findWithDefault root "Root" dict)
      ]
>>>>>>> 2cda7c92fcf859f588ef19db6e288dd3ad74727d:src/Pdf/Object/Encode.hs
    )
  )
updateTrailer _ _ object = object

removeUnused :: PDFDocument -> PDFDocument
removeUnused doc = dFilter used doc
 where
  used :: PDFObject -> Bool
  used object = isNotLinearized object && isReferenced object

  isNotLinearized :: PDFObject -> Bool
  isNotLinearized = not . hasKey "Linearized"

  isReference :: PDFObject -> Bool
  isReference PDFReference{}  = True
  isReference _anyOtherObject = False

  references :: PDFDocument
  references = deepFind isReference (uncompress doc)

  isReferenced :: PDFObject -> Bool
  isReferenced (PDFIndirectObject num gen _) =
    PDFReference num gen `elem` references
  isReferenced (PDFIndirectObjectWithStream num gen _ _) =
    PDFReference num gen `elem` references
  isReferenced (PDFObjectStream num gen _ _) =
    PDFReference num gen `elem` references
  isReferenced _anyOtherObject = True

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
  :: StepM m
  => [PDFObject] -- ^ A list of PDF objects (order matters)
  -> StepT m BS.ByteString -- ^ A unified error or a bytestring
pdfEncode objects
  | null (ppIndirectObjects partObjects) = do
    stepT "Indirect objects not found"
    throwError EncodeNoIndirectObject
  | null (ppHeads partObjects) = do
    stepT "Version not found"
    throwError EncodeNoVersion
  | null (ppTrailers partObjects) = do
    stepT "Trailer not found"
    throwError EncodeNoTrailer
  | otherwise = do
    stepT "Encoding PDF"
    optimizeds <- forM (ppIndirectObjects partObjects) optimize

    stepT "Building optimized PDF"
    let
      encodeds    = OS.fromMostRecents (encodeObject <$> optimizeds)
      body        = BS.concat $ eoBinaryData <$> OS.toAscList encodeds
      xref        = xrefTable (BS.length pdfHead) encodeds
      encodedXRef = fromPDFObject xref
      startxref =
        fromPDFObject (PDFStartXRef (BS.length pdfHead + BS.length body))
      trailer = fromPDFObject $ updateTrailer
        (case findRoot objects of
          Nothing   -> PDFNumber 0
          Just root -> root
        )
        (xrefCount xref)
        pdfTrailer

    return $ BS.concat [pdfHead, body, encodedXRef, trailer, startxref, pdfEnd]
 where
  partObjects = foldr ((<>) . toPartition) mempty (removeUnused objects)
  pdfTrailer  = lastTrailer partObjects

  pdfHead     = (fromPDFObject . firstVersion) partObjects
  pdfEnd      = fromPDFObject PDFEndOfFile
