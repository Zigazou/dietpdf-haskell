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
import qualified Data.IntMap.Strict            as IM
import           Data.Ix                        ( range
                                                , rangeSize
                                                )
import           Data.Sort                      ( sort )
import           Pdf.Object.Object              ( PDFObject(PDFXRef)
                                                , XRefSubsection(XRefSubsection)
                                                , freeEntry
                                                , inUseEntry
                                                )
import           Pdf.Document.Collection        ( PDFObjects
                                                , EncodedObjects
                                                , ObjectOffsets
                                                , EncodedObject
                                                  ( EncodedObject
                                                  , eoBinaryData
                                                  )
                                                , encodeObject
                                                )
import           Pdf.Document.Document          ( toList )

-- | Given a collection of encoded objects, calculates their offsets
calcOffsets :: Int -> EncodedObjects -> ObjectOffsets
calcOffsets startOffset = snd . calcOffset . sort . toList
 where
  calcOffset :: [EncodedObject] -> (Int, ObjectOffsets)
  calcOffset = foldl'
    (\(offset, offsets) (EncodedObject number objectLength _) ->
      (offset + objectLength, IM.insert number offset offsets)
    )
    (startOffset, IM.empty)

-- | Given a collection of encoded objects, generates an old format XRef table
xrefTable :: Int -> EncodedObjects -> PDFObject
xrefTable startOffset objects | objects == mempty = PDFXRef []
                              | otherwise         = PDFXRef [xrefSubsection]
 where
  offsets  = calcOffsets startOffset objects
  numRange = if IM.null offsets
    then (0, 0)
    else IM.foldlWithKey'
      (\(mini, maxi) objNum _ -> (min objNum mini, max objNum maxi))
      (maxBound, minBound)
      offsets
  entries =
    [ maybe freeEntry (`inUseEntry` 0) (IM.lookup index offsets)
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
