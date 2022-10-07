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
import           Pdf.Document.Document          ( PDFDocument
                                                , dFilter
                                                , findLast
                                                , cMap
                                                , deepFind
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
import           Pdf.Document.Collection        ( encodeObject
                                                , eoBinaryData
                                                )
import           Pdf.Document.Uncompress        ( uncompress )

updateTrailer :: PDFObject -> Int -> PDFObject -> PDFObject
updateTrailer root entriesCount (PDFTrailer (PDFDictionary dict)) = PDFTrailer
  (PDFDictionary
    (Map.fromList
      [ ("Size", PDFNumber (fromIntegral entriesCount))
      , ("Root", Map.findWithDefault root "Root" dict)
      ]
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
  :: PDFDocument -- ^ A `PDFDocument` containing `PDFObject` (order matters)
  -> Either UnifiedError BS.ByteString -- ^ A unified error or the bytestring
pdfEncode objects
  | null (ppIndirectObjects partObjects) = Left EncodeNoIndirectObject
  | null (ppHeads partObjects) = Left EncodeNoVersion
  | null (ppTrailers partObjects) = Left EncodeNoTrailer
  | otherwise = Right
  $ BS.concat [pdfHead, body, encodedXRef, trailer, startxref, pdfEnd]
 where
  partObjects = foldr ((<>) . toPartition) mempty (removeUnused objects)
  pdfTrailer  = lastTrailer partObjects

  pdfHead     = (fromPDFObject . firstVersion) partObjects
  pdfEnd      = fromPDFObject PDFEndOfFile

  encodeds    = cMap (encodeObject . optimize) (ppIndirectObjects partObjects)
  body        = foldMap eoBinaryData encodeds

  xref        = xrefTable encodeds
  encodedXRef = fromPDFObject xref
  trailer     = fromPDFObject $ updateTrailer
    (case findRoot objects of
      Nothing   -> PDFNumber 0
      Just root -> root
    )
    (xrefCount xref)
    pdfTrailer

  totalLength = BS.length pdfHead + BS.length body
  startxref   = fromPDFObject (PDFStartXRef totalLength)
