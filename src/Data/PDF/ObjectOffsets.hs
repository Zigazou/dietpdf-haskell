{-|
Utilities for maps of object offsets.

This module provides a small API around a mapping from PDF object numbers to
their corresponding offsets/entries.

It is primarily used when working with cross-reference information, where
object numbers are expected to form a contiguous range and missing entries are
treated as free.
-}
module Data.PDF.ObjectOffsets
  ( ObjectOffsets
  , indexRange
  , insertFreeEntries
  )
where

import Data.IntMap.Strict (IntMap, findMax, findMin, fromAscList, toAscList)
import Data.Kind (Type)

import Data.PDF.ObjectOffset (ObjectOffset (FreeEntry))

{-|
A collection of object offsets indexed by object number.

This is typically backed by an 'IntMap' for efficient lookup.
-}
type ObjectOffsets :: Type
type ObjectOffsets = IntMap ObjectOffset

{-|
Return the minimum and maximum object numbers present in the map.

Precondition: the map must be non-empty.
-}
indexRange :: ObjectOffsets -> (Int, Int)
indexRange offsets = (fst $ findMin offsets, fst $ findMax offsets)

{-|
Insert missing object numbers as free entries.

Given a sparse map of object offsets, this fills any gaps between existing
entries by inserting 'FreeEntry' values, yielding a map whose keys form a
contiguous sequence between the existing minimum and maximum keys.

The values for already-present keys are preserved.
-}
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
