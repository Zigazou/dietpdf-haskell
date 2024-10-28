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

{- |
A translation table is a map from values to values.
-}
type TranslationTable :: Type -> Type
type TranslationTable a = Map a a

{- |
Create a translation table from a list of pairs.
-}
mkTranslationTable :: Ord a => [(a, a)] -> TranslationTable a
mkTranslationTable = fromList

{- |
Convert a value using a translation table.

If the value is not in the table, it returns the original value.
-}
convert :: Ord a => TranslationTable a -> a -> a
convert table value = fromMaybe value (Map.lookup value table)

shortFirst :: (Ord a, HasLength a) => a -> a -> Ordering
shortFirst x y | xLength /= yLength = compare xLength yLength
               | otherwise          = compare x y
 where
  xLength = objectLength x
  yLength = objectLength y

{- |
Rename a list of strings.
The corresponding `Map` is returned.

Shortest strings are renamed first, giving them the shortest name.
-}
getTranslationTable
  :: (Ord a, HasLength a, Foldable t)
  => (a -> Int -> a)
  -> t a
  -> TranslationTable a
getTranslationTable = getTranslationTableFrom 0

{- |
Rename a list of strings.
The corresponding `Map` is returned.

Shortest strings are renamed first, giving them the shortest name.
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

hasTerm :: Ord a => TranslationTable a -> a -> Bool
hasTerm = flip Map.member

{- |
Merge two translation tables.
-}
mergeTranslationTables
  :: Ord a
  => TranslationTable a
  -> TranslationTable a
  -> TranslationTable a
mergeTranslationTables = Map.union

{- |
Get the next free index in a translation table.
-}
nextFreeIndex :: TranslationTable a -> Int
nextFreeIndex = Map.size
