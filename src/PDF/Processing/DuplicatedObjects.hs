module PDF.Processing.DuplicatedObjects
  ( findDuplicatedObjects
  , DuplicatedObjects(DuplicatedObjects)
  , Duplicates(Duplicates, dHash, dDuplicates)
  , hasNoDuplicates
  , hasDuplicates
  , duplicateCount
  , convertDuplicatedReferences
  ) where

import           Data.Context            (Contextual (ctx))
import           Data.Kind               (Type)
import           Data.Logging            (Logging)
import           Data.Map                (Map)
import Data.Map qualified                as Map
import           Data.Maybe              (fromMaybe)
import           Data.PDF.ObjectHash     (ObjectHash, objectHash)
import           Data.PDF.PDFObject      (PDFObject (PDFIndirectObjectWithStream, PDFReference))
import           Data.PDF.PDFPartition   (PDFPartition (PDFPartition))
import           Data.PDF.PDFWork        (PDFWork, sayP, withContext, modifyIndirectObjectsP)
import           Data.Set                (Set)
import Data.Set  qualified               as Set

import           PDF.Document.Uncompress (uncompressObjects)
import Data.List (sort)
import Data.Foldable (foldl')
import PDF.Processing.PDFWork (deepMapP)

type Duplicates :: Type
data Duplicates = Duplicates
  { dHash       :: !ObjectHash -- ^ Hash of the stream content.
  , dDuplicates :: !(Set Int)
    -- ^ Set of object numbers that share the same stream hash.
  }

instance Eq Duplicates where
  (Duplicates hash1 _) == (Duplicates hash2 _) = hash1 == hash2

hasNoDuplicates :: Duplicates -> Bool
hasNoDuplicates = (<= 1) . Set.size . dDuplicates

hasDuplicates :: Duplicates -> Bool
hasDuplicates = (> 1) . Set.size . dDuplicates

originalAndDuplicates :: Duplicates -> (Int, [Int])
originalAndDuplicates (Duplicates _hash objects) =
  case sort (Set.toList objects) of
    (original:remains) -> (original, remains)
    []                 -> (-1, [])

type DuplicatedObjects :: Type
newtype DuplicatedObjects =
  DuplicatedObjects (Map ObjectHash Duplicates)

instance Show DuplicatedObjects where
  show (DuplicatedObjects mp) =
    "DuplicatedObjects: " ++ show (dDuplicates <$> Map.elems mp)

duplicateCount :: DuplicatedObjects -> Int
duplicateCount (DuplicatedObjects mp) =
  let originalCount = Map.size mp
      totalCount = Set.size . dDuplicates <$> Map.elems mp
   in sum totalCount - originalCount

{-|
Find duplicated objects in a PDF partition based on their stream content hash.
-}
findDuplicatedObjects
  :: Logging m
  => PDFPartition
  -> PDFWork m DuplicatedObjects
findDuplicatedObjects (PDFPartition objectsWithStream _ _ _) =
  withContext (ctx ("findDuplicatedObjects" :: String)) $ do
    sayP "Finding duplicated objects"
    uObjectsWithStream <- uncompressObjects objectsWithStream
    return
      (DuplicatedObjects
         $ Map.filter hasDuplicates
         $ foldl updateDuplicates mempty uObjectsWithStream)
  where
    updateDuplicates
      :: Map ObjectHash Duplicates
      -> PDFObject
      -> Map ObjectHash Duplicates
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

buildReferenceMap :: DuplicatedObjects -> Map Int Int
buildReferenceMap (DuplicatedObjects mp) = Map.foldl' insertReferences mempty mp
 where
  insertReferences :: Map Int Int -> Duplicates -> Map Int Int
  insertReferences acc duplicates =
    let (original, duplicatesList) = originalAndDuplicates duplicates
    in foldl' (flip (`Map.insert` original)) acc duplicatesList

{-|
Converts references to duplicated objects to point to a single original object.
-}
convertDuplicatedReferences
  :: Logging m
  => DuplicatedObjects
  -> PDFWork m ()
convertDuplicatedReferences duplicatedObjects =
  withContext (ctx ("convertDuplicatedReferences" :: String)) $ do
    sayP "Converting duplicated object references"
    modifyIndirectObjectsP convertAllReferences
 where
  referenceMap :: Map Int Int
  referenceMap = buildReferenceMap duplicatedObjects

  convertReference :: Logging m => PDFObject -> PDFWork m PDFObject
  convertReference (PDFReference objNum genNum) =
    case Map.lookup objNum referenceMap of
      Just originalNum -> return $ PDFReference originalNum genNum
      Nothing -> return $ PDFReference objNum genNum
  convertReference otherObject = return otherObject

  convertAllReferences :: Logging m => PDFObject -> PDFWork m PDFObject
  convertAllReferences = deepMapP convertReference
