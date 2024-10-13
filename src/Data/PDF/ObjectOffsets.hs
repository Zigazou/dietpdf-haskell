module Data.PDF.ObjectOffsets
  ( ObjectOffsets
  , indexRange
  , insertFreeEntries
  )
where

import Data.IntMap.Strict (IntMap, findMax, findMin, fromAscList, toAscList)
import Data.Kind (Type)

import Data.PDF.ObjectOffset (ObjectOffset (FreeEntry))

-- | A collection of object offsets indexed by the object number
type ObjectOffsets :: Type
type ObjectOffsets = IntMap ObjectOffset

indexRange :: ObjectOffsets -> (Int, Int)
indexRange offsets = (fst $ findMin offsets, fst $ findMax offsets)

insertFreeEntries :: ObjectOffsets -> ObjectOffsets
insertFreeEntries offsets = fromAscList $ go (toAscList offsets)
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
