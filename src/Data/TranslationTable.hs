{-|
Translation tables for renaming values with generation and lookup utilities.

Provides mapping construction, value translation, and semantic operations such
as renaming based on a generation function while prioritizing shorter names.
-}
module Data.TranslationTable
  ( TranslationTable
  , mkTranslationTable
  , convert
  , shortFirst
  , getTranslationTable
  , getTranslationTableFrom
  , mergeTranslationTables
  , nextFreeIndex
  , hasTerm
  ) where

import Control.Monad (liftM2)

import Data.Foldable (toList)
import Data.HasLength (HasLength (objectLength))
import Data.Kind (Type)
import Data.List (sortBy)
import Data.List.Extra (nubOrd)
import Data.Map (Map, fromList)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

{-|
Translation table: a bidirectional mapping from values to values.

Used to store renamed or transformed mappings of elements, typically for
compression or canonicalization.
-}
type TranslationTable :: Type -> Type
type TranslationTable a = Map a a

{-|
Create a translation table from a list of key-value pairs.

Entries are inserted into the underlying 'Map'; later duplicates override
earlier ones.
-}
mkTranslationTable :: Ord a => [(a, a)] -> TranslationTable a
mkTranslationTable = fromList

{-|
Translate a value using the given table.

If the value appears as a key in the table, its corresponding value is returned.
Otherwise, the original value is returned unchanged.
-}
convert :: Ord a => TranslationTable a -> a -> a
convert table value = fromMaybe value (Map.lookup value table)

{-|
Comparison function that orders by length first, then by value.

Used to sort containers so shorter ones are prioritized for renaming/indexing.
-}
shortFirst :: (Ord a, HasLength a) => a -> a -> Ordering
shortFirst x y | xLength /= yLength = compare xLength yLength
               | otherwise          = compare x y
 where
  xLength = objectLength x
  yLength = objectLength y

{-|
Generate a translation table by renaming elements, starting from index 0.

Removes duplicates and sorts by length (shortest first) to assign optimal
names. The generator function takes an element and an index and produces its
renamed form.
-}
getTranslationTable
  :: (Ord a, HasLength a, Foldable t)
  => (a -> Int -> a)
  -> t a
  -> TranslationTable a
getTranslationTable = getTranslationTableFrom 0

{-|
Generate a translation table by renaming elements, starting from a custom index.

Removes duplicates and sorts by length (shortest first) to assign optimal names.
The generator function takes an element and its 0-based index (offset by 'from')
and produces its renamed form.
-}
getTranslationTableFrom
  :: (Ord a, HasLength a, Foldable t)
  => Int
  -> (a -> Int -> a)
  -> t a
  -> TranslationTable a
getTranslationTableFrom from generator names =
  let terms = (sortBy shortFirst . nubOrd . toList) names
  in mkTranslationTable $ zipWith (liftM2 (.) (,) generator) terms [from..]

{-|
Check whether a value is a key in the translation table.
-}
hasTerm :: Ord a => TranslationTable a -> a -> Bool
hasTerm = flip Map.member

{-|
Combine two translation tables using left-biased union.

If both tables contain a mapping for the same key, the mapping from the first
table is retained.
-}
mergeTranslationTables
  :: Ord a
  => TranslationTable a
  -> TranslationTable a
  -> TranslationTable a
mergeTranslationTables = Map.union

{-|
Return the number of entries in the translation table.

Useful for determining the next available index when generating new names.
-}
nextFreeIndex :: TranslationTable a -> Int
nextFreeIndex = Map.size
