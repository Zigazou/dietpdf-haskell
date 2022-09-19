{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
module Pdf.Object.Encode
  ( -- * Encoding
    pdfEncode
    -- * XRef generation
  , calcOffsets
  , encodeObject
  ) where

import qualified Data.ByteString               as BS
import qualified Data.HashMap.Strict           as HM
import qualified Data.Set.Ordered              as OS
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFDictionary
                                                  , PDFEndOfFile
                                                  , PDFNumber
                                                  , PDFIndirectObject
                                                  , PDFStartXRef
                                                  , PDFTrailer
                                                  )
                                                , fromPDFObject
                                                , xrefCount
                                                , getValue
                                                )
import           Pdf.Object.XRef                ( calcOffsets
                                                , xrefTable
                                                )
import           Pdf.Object.Partition           ( PDFPartition
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
import qualified Util.OrderedSet               as OS
import           Pdf.Object.Optimize            ( optimize )
import           Data.Maybe                     ( isNothing )
import           Pdf.Object.Collection          ( findLastValue
                                                , encodeObject
                                                , eoBinaryData
                                                )

updateTrailer :: PDFObject -> Int -> PDFObject -> PDFObject
updateTrailer root entriesCount (PDFTrailer (PDFDictionary dict)) = PDFTrailer
  (PDFDictionary
    (HM.fromList
      [ ("Size", PDFNumber (fromIntegral entriesCount))
      , ("Root", HM.lookupDefault root "Root" dict)
      ]
    )
  )
updateTrailer _ _ object = object

removeUnused :: [PDFObject] -> [PDFObject]
removeUnused = filter noLinearized
 where
  noLinearized :: PDFObject -> Bool
  noLinearized (PDFIndirectObject _ _ (PDFDictionary dictionary) _) =
    isNothing $ dictionary HM.!? "Linearized"
  noLinearized _ = True

findRoot :: [PDFObject] -> Maybe PDFObject
findRoot = findLastValue (getValue "Root")

{-|
Given a list of PDF objects, generate the PDF file content.

This function recreates the XRef table in the old format.

An error is signaled in the following cases:

- no numbered objects in the list of PDF objects
- no PDF version in the list of PDF objects
- no trailer in the list of PDF objects
-}
pdfEncode
  :: [PDFObject] -- ^ A list of PDF objects (order matters)
  -> Either UnifiedError BS.ByteString -- ^ A unified error or the bytestring
pdfEncode objects
  | null (ppIndirectObjects partObjects) = Left EncodeNoIndirectObject
  | null (ppHeads partObjects) = Left EncodeNoVersion
  | null (ppTrailers partObjects) = Left EncodeNoTrailer
  | otherwise = Right
  $ BS.concat [pdfHead, body, encodedXRef, trailer, startxref, pdfEnd]
 where
  partObjects = mconcat (toPartition <$> removeUnused objects)
  pdfTrailer  = lastTrailer partObjects

  pdfHead     = (fromPDFObject . firstVersion) partObjects
  pdfEnd      = fromPDFObject PDFEndOfFile

  encodeds    = OS.fromMostRecents
    (encodeObject . optimize <$> ppIndirectObjects partObjects)
  body        = BS.concat $ eoBinaryData <$> OS.toAscList encodeds

  xref        = xrefTable encodeds
  encodedXRef = fromPDFObject xref
  trailer     = fromPDFObject $ updateTrailer
    (case findRoot objects of
      Nothing   -> PDFNumber 0
      Just root -> root
    )
    (xrefCount xref)
    pdfTrailer

  totalLength =
    BS.length pdfHead
      + BS.length body
      + BS.length encodedXRef
      + BS.length trailer
  startxref = fromPDFObject (PDFStartXRef totalLength)
