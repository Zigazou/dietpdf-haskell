{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
module Pdf.Document.XRef
  ( -- * XRef generation
    calcOffsets
  , encodeObject
  , xrefTable
  , xrefStreamTable
  , PDFObjects
  , EncodedObject(..)
  , EncodedObjects
  , ObjectOffsets
  ) where

import           Data.Foldable                  ( foldl' )
import qualified Data.HashMap.Strict           as HM
import           Data.Ix                        ( range
                                                , rangeSize
                                                )
import qualified Data.Set.Ordered              as OS
import           Data.Sort                      ( sort )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFDictionary
                                                  , PDFIndirectObject
                                                  , PDFEndOfFile
                                                  , PDFNumber
                                                  , PDFIndirectObject
                                                  , PDFStartXRef
                                                  , PDFTrailer
                                                  , PDFXRef
                                                  )
                                                , XRefSubsection(XRefSubsection)
                                                , freeEntry
                                                , fromPDFObject
                                                , inUseEntry
                                                , xrefCount
                                                , getValue
                                                )
import           Pdf.Object.Collection          ( PDFObjects
                                                , EncodedObjects
                                                , ObjectOffsets
                                                , EncodedObject
                                                  ( EncodedObject
                                                  , eoBinaryData
                                                  )
                                                , encodeObject
                                                )


-- | Given a collection of encoded objects, calculates their offsets
calcOffsets :: EncodedObjects -> ObjectOffsets
calcOffsets = snd . calcOffset . sort . OS.toAscList
 where
  calcOffset :: [EncodedObject] -> (Int, ObjectOffsets)
  calcOffset = foldl'
    (\(offset, offsets) (EncodedObject number objectLength _) ->
      (offset + objectLength, HM.insert number offset offsets)
    )
    (0, HM.empty)

-- | Given a collection of encoded objects, generates an old format XRef table
xrefTable :: EncodedObjects -> PDFObject
xrefTable objects | OS.null objects = PDFXRef []
                  | otherwise       = PDFXRef [xrefSubsection]
 where
  offsets  = calcOffsets objects
  numRange = if HM.null offsets
    then (0, 0)
    else HM.foldlWithKey'
      (\(mini, maxi) objNum _ -> (min objNum mini, max objNum maxi))
      (maxBound, minBound)
      offsets
  entries =
    [ maybe freeEntry (`inUseEntry` 0) (HM.lookup index offsets)
    | index <- range numRange
    ]
  xrefSubsection = XRefSubsection (fst numRange) (rangeSize numRange) entries

-- | Given a collection of encoded objects, generates a cross-reference stream.
xrefStreamTable :: Int -> EncodedObjects -> PDFObject
xrefStreamTable = error "todo"
{-
xrefStreamTable number objects
    | OS.null objects = PDFIndirectObject number 0 []
    | otherwise       = PDFIndirectObject number 0 [xrefSubsection]
    fromIndirectObject number revision object (Just stream) = BS.concat

 where
  offsets  = calcOffsets objects
  numRange = if HM.null offsets
    then (0, 0)
    else HM.foldlWithKey'
      (\(mini, maxi) objNum _ -> (min objNum mini, max objNum maxi))
      (maxBound, minBound)
      offsets
  entries =
    [ maybe freeEntry (`inUseEntry` 0) (HM.lookup index offsets)
    | index <- range numRange
    ]
  xrefSubsection = XRefSubsection (fst numRange) (rangeSize numRange) entries
-}