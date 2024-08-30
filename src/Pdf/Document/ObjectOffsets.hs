module Pdf.Document.ObjectOffsets
  ( ObjectOffsets
  , indexRange
  , insertFreeEntries
  )
where

import Data.IntMap.Strict qualified as IM
import Data.Kind (Type)

import Pdf.Document.ObjectOffset (ObjectOffset (FreeEntry))

-- | A collection of object offsets indexed by the object number
type ObjectOffsets :: Type
type ObjectOffsets = IM.IntMap ObjectOffset

indexRange :: ObjectOffsets -> (Int, Int)
indexRange offsets = (fst $ IM.findMin offsets, fst $ IM.findMax offsets)

insertFreeEntries :: ObjectOffsets -> ObjectOffsets
insertFreeEntries offsets = IM.fromAscList $ go (IM.toAscList offsets)
  where
    go [] = []
    go [(number, offset)] = [(number, offset)]
    go ((currNumber, currOffset) : (nextNumber, nextOffset) : rest)
      | nextNumber == currNumber + 1 = (currNumber, currOffset)
                                     : go ((nextNumber, nextOffset): rest)
      | otherwise = (currNumber, currOffset)
                  : go ( (currNumber + 1, FreeEntry (currNumber + 1))
                       : (nextNumber, nextOffset)
                       : rest
                       )
