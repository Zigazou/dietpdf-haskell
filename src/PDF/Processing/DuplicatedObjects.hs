{- |
This module provides functionality to detect and eliminate duplicated objects in
PDF documents based on their content hash. Duplicated objects are common in PDF
files and removing them can significantly reduce file size.

The module works by:

1. Computing content hashes for all objects with streams
2. Identifying objects that share the same hash
3. Building a reference map to replace duplicate references with a single
   original
4. Converting all references throughout the document

Example usage:

@ partition <- getPDFPartition document duplicates <- findDuplicatedObjects
partition convertDuplicatedReferences duplicates @
-}
module PDF.Processing.DuplicatedObjects
  ( findDuplicatedObjects
  , DuplicatedObjects (DuplicatedObjects)
  , Duplicates (Duplicates, dHash, dDuplicates)
  , hasDuplicates
  , duplicateCount
  , convertDuplicatedReferences
  )
where

import Data.Context (Contextual (ctx))
import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.List (sort)
import Data.Logging (Logging)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.PDF.ObjectHash (ObjectHash, objectHash)
import Data.PDF.PDFObject
  (PDFObject (PDFIndirectObjectWithStream, PDFReference))
import Data.PDF.PDFPartition (PDFPartition (PDFPartition))
import Data.PDF.PDFWork (PDFWork, modifyIndirectObjectsP, sayP, withContext)
import Data.Set (Set)
import Data.Set qualified as Set

import PDF.Document.Uncompress (uncompressObjects)
import PDF.Processing.PDFWork (deepMapP)

{-|
Represents a group of objects that share the same content hash.

This type tracks objects that are duplicates of each other based on their stream
content. It stores both the hash value and the set of object numbers that share
this hash.
-}
type Duplicates :: Type
data Duplicates = Duplicates
  { -- | Hash of the stream content.
    dHash :: !ObjectHash,
    -- | Set of object numbers that share the same stream hash.
    dDuplicates :: !(Set Int)
  }

{-|
Two 'Duplicates' are equal if they have the same hash, regardless of which
specific objects share that hash.
-}
instance Eq Duplicates where
  (Duplicates hash1 _) == (Duplicates hash2 _) = hash1 == hash2

{-|
Check if a 'Duplicates' group contains actual duplicates.

Returns 'True' if there are multiple objects sharing the same hash, indicating
duplication that can be eliminated.
-}
hasDuplicates :: Duplicates -> Bool
hasDuplicates = (> 1) . Set.size . dDuplicates

{-|
Extract the original object number and the list of duplicate object numbers.

The original is chosen as the object with the lowest object number. Returns (-1,
[]) if the set is empty.
-}
originalAndDuplicates :: Duplicates -> (Int, [Int])
originalAndDuplicates (Duplicates _hash objects) =
  case sort (Set.toList objects) of
    (original : remains) -> (original, remains)
    []                   -> (-1, [])

{-|
Collection of all duplicated objects in a PDF document.

Maps content hashes to groups of objects that share that hash. Only includes
hashes that have actual duplicates (2 or more objects).
-}
type DuplicatedObjects :: Type
newtype DuplicatedObjects
  = DuplicatedObjects (Map ObjectHash Duplicates)

instance Show DuplicatedObjects where
  show :: DuplicatedObjects -> String
  show (DuplicatedObjects mp) =
    "DuplicatedObjects: " ++ show (dDuplicates <$> Map.elems mp)

{-|
Count the total number of duplicate objects that can be eliminated.

This counts how many objects can be removed by deduplication. For example, if
there are 3 objects with hash A and 2 objects with hash B, the count is 3 (2
duplicates of A + 1 duplicate of B), since we keep one original of each unique
hash.
-}
duplicateCount :: DuplicatedObjects -> Int
duplicateCount (DuplicatedObjects mp) =
  let originalCount = Map.size mp
      totalCount = Set.size . dDuplicates <$> Map.elems mp
   in sum totalCount - originalCount

{-|
Find duplicated objects in a PDF partition based on their stream content hash.

This function analyzes all objects with streams in a PDF partition, computes
their content hashes, and identifies groups of objects that are duplicates of
each other. Objects are considered duplicates if they have identical stream
content after decompression.

The process involves:

1. Uncompressing all stream objects to get their raw content
2. Computing a hash for each object's content
3. Grouping objects by their hash value
4. Filtering to keep only groups with multiple objects

Returns a 'DuplicatedObjects' structure containing all groups of duplicates
found in the document.
-}
findDuplicatedObjects ::
  (Logging m) =>
  PDFPartition ->
  PDFWork m DuplicatedObjects
findDuplicatedObjects (PDFPartition objectsWithStream _ _ _) =
  withContext (ctx ("findDuplicatedObjects" :: String)) $ do
    sayP "Finding duplicated objects"
    uObjectsWithStream <- uncompressObjects objectsWithStream
    return
      ( DuplicatedObjects $
          Map.filter hasDuplicates $
            foldl updateDuplicates mempty uObjectsWithStream
      )
  where
    {-
    Update the duplicates map with a new object.
    
    If an object with the same hash already exists, adds the new object to that
    hash's set. Otherwise, creates a new entry.
    -}
    updateDuplicates ::
      Map ObjectHash Duplicates ->
      PDFObject ->
      Map ObjectHash Duplicates
    updateDuplicates duplicates object@(PDFIndirectObjectWithStream objectNumber _ _ _) =
      let streamHash = fromMaybe mempty (objectHash object)
          currentObject = Duplicates streamHash (Set.singleton objectNumber)
       in case Map.lookup streamHash duplicates of
            Just (Duplicates _hash existingSet) ->
              let newSet = Set.insert objectNumber existingSet
               in Map.insert
                    streamHash
                    (Duplicates streamHash newSet)
                    duplicates
            _anyOtherCase -> Map.insert streamHash currentObject duplicates
    updateDuplicates duplicates _anyOtherObject = duplicates

{-|
Build a reference map from duplicate object numbers to original object numbers.

For each group of duplicates, this creates mappings from each duplicate's object
number to the original (lowest-numbered) object. This map is used to rewrite all
references throughout the document.

For example, if objects 5, 12, and 19 are duplicates, the map will contain: 12
-> 5 and 19 -> 5
-}
buildReferenceMap :: DuplicatedObjects -> Map Int Int
buildReferenceMap (DuplicatedObjects mp) = Map.foldl' insertReferences mempty mp
  where
    -- Insert reference mappings for all duplicates in a group.
    insertReferences :: Map Int Int -> Duplicates -> Map Int Int
    insertReferences acc duplicates =
      let (original, duplicatesList) = originalAndDuplicates duplicates
       in foldl' (flip (`Map.insert` original)) acc duplicatesList

{-|
Converts all references to duplicated objects to point to a single original
object.

This function performs the actual deduplication by rewriting all PDF references
throughout the document. For each reference to a duplicate object, it replaces
it with a reference to the original (lowest-numbered) object.

This is a deep transformation that traverses all indirect objects and their
contents, ensuring that every reference is updated. After this operation, all
duplicate objects become unreferenced and can be safely removed from the
document.

Note: This function only rewrites references; it does not remove the duplicate
objects themselves. That should be done in a separate garbage collection step.
-}
convertDuplicatedReferences ::
  (Logging m) =>
  DuplicatedObjects ->
  PDFWork m ()
convertDuplicatedReferences duplicatedObjects =
  withContext (ctx ("convertDuplicatedReferences" :: String)) $ do
    sayP "Converting duplicated object references"
    modifyIndirectObjectsP convertAllReferences
  where
    -- Map from duplicate object numbers to their original object numbers.
    referenceMap :: Map Int Int
    referenceMap = buildReferenceMap duplicatedObjects

    -- Convert a single reference if it points to a duplicate object.
    convertReference :: (Logging m) => PDFObject -> PDFWork m PDFObject
    convertReference (PDFReference objNum genNum) =
      case Map.lookup objNum referenceMap of
        Just originalNum -> return $ PDFReference originalNum genNum
        Nothing          -> return $ PDFReference objNum genNum
    convertReference otherObject = return otherObject

    -- Apply reference conversion recursively to all objects in the structure.
    convertAllReferences :: (Logging m) => PDFObject -> PDFWork m PDFObject
    convertAllReferences = deepMapP convertReference
