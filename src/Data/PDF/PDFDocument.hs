{-|
Ordered collections of PDF objects.

This module defines 'PDFDocument' as an insertion-ordered collection of
'PDFObject' values with no duplicates.

The underlying representation is an ordered set. As a consequence, most
operations require an 'Ord' constraint and behave like set operations while
preserving insertion order.

The helper type 'CollectionOf' generalizes this notion to any element type.
-}
module Data.PDF.PDFDocument
  ( PDFDocument
  , CollectionOf(CollectionOf)
  , singleton
  , fromList
  , toList
  , cSize
  , cCons
  , cfMap
  , cFilter
  , lMap
  , dSepBy1
  , dSepBy
  , deepFind
  , dFindLast
  , member
  ) where

import Control.Applicative (Alternative, (<|>))

import Data.Fallible (FallibleT)
import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.PDF.PDFObject
  ( PDFObject (PDFArray, PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream, PDFObjectStream, PDFTrailer)
  )
import Data.Set.Ordered qualified as OS

{-|
This is a simple trick to allow `PDFDocument` to be foldable because it is not
of kind '* -> *' as required by the Foldable class.
-}
type role CollectionOf nominal
type CollectionOf :: Type -> Type
newtype CollectionOf a = CollectionOf (OS.OSet a) deriving stock (Eq, Show)

{-|
A `PDFDocument` is a collection of `PDFObject` keeping the insertion order and
having no duplicates.

Every function working on `CollectionOf` works on `PDFDocument`.
-}
type PDFDocument :: Type
type PDFDocument = CollectionOf PDFObject

{-|
Check whether an object is a member of a collection.
-}
member :: Ord a => a -> CollectionOf a -> Bool
member object (CollectionOf objects) = OS.member object objects

instance Foldable CollectionOf where
  foldMap :: Monoid m => (a -> m) -> CollectionOf a -> m
  foldMap f (CollectionOf dict) = foldMap f dict

{-|
Concatenate two collections.

This keeps insertion order and removes duplicates according to 'Ord'.
-}
instance Ord a => Semigroup (CollectionOf a) where
  (<>) :: CollectionOf a -> CollectionOf a -> CollectionOf a
  (<>) (CollectionOf x) (CollectionOf y) = CollectionOf (x OS.<>| y)

instance Ord a => Monoid (CollectionOf a) where
  mempty :: CollectionOf a
  mempty = CollectionOf OS.empty

{-|
Get the size of a `CollectionOf`.
-}
cSize :: CollectionOf a -> Int
cSize (CollectionOf os) = OS.size os

{-|
A `map` function for objects of type `CollectionOf` which works in a
`FallibleT` monad.
-}
cfMap
  :: (Ord b, Monad m)
  => (a -> FallibleT m b)
  -> CollectionOf a
  -> FallibleT m (CollectionOf b)
cfMap f = foldr (accumulate f) (pure mempty)
 where
  accumulate
    :: (Monad m, Ord b)
    => (a -> FallibleT m b)
    -> a
    -> FallibleT m (CollectionOf b)
    -> FallibleT m (CollectionOf b)
  accumulate transform !a !fc = do
    collection <- fc
    result     <- transform a
    return (cCons result collection)

{-|
Equivalent to the `filter` function which works on `List` except this one
works on `CollectionOf`.
-}
cFilter :: Ord a => (a -> Bool) -> CollectionOf a -> CollectionOf a
cFilter f (CollectionOf !set) = CollectionOf $! OS.filter f set

{-|
A `map` function for objects of type `CollectionOf` which converts to a `List`.

This allows to get rid of the `Ord` constraint of cMap.

The resulting list respects the collection's ordering.
-}
lMap :: (a -> b) -> CollectionOf a -> [b]
lMap f (CollectionOf !objects) = (fmap f . OS.toAscList) objects

{-|
Create a `PDFDocument` with only one `PDFObject`.
-}
singleton :: PDFObject -> PDFDocument
singleton = CollectionOf . OS.singleton

{-|
Build a collection from a list.

Duplicates are removed by the underlying ordered set (according to 'Ord') while
preserving insertion order.
-}
fromList :: Ord a => [a] -> CollectionOf a
fromList = CollectionOf . foldl' (OS.>|) OS.empty

{-|
Convert a `CollectionOf` to a `List`.
-}
toList :: CollectionOf a -> [a]
toList (CollectionOf !objects) = OS.toAscList objects

{-|
The cons function for `CollectionOf` type.
-}
cCons :: Ord a => a -> CollectionOf a -> CollectionOf a
cCons object (CollectionOf !objects) = CollectionOf (object OS.<| objects)

{-|
The `sepBy1` function for `CollectionOf` (`sepBy1` only generates `List`).
-}
dSepBy1 :: (Alternative f, Ord a) => f a -> f s -> f (CollectionOf a)
dSepBy1 !p !s = go where go = liftA2 cCons p ((s *> go) <|> pure mempty)

{-|
The `sepBy` function for `CollectionOf` (`sepBy` only generates `List`).
-}
dSepBy :: (Alternative f, Ord a) => f a -> f s -> f (CollectionOf a)
dSepBy !p !s =
  liftA2 cCons p ((s *> dSepBy1 p s) <|> pure mempty) <|> pure mempty

{-|
Find last value in a `CollectionOf` satisfying a predicate.

If the predicate is never satisfied, the function returns `Nothing`.
-}
dFindLast :: (a -> Bool) -> CollectionOf a -> Maybe a
dFindLast p =
  foldr (\object found -> if p object then Just $! object else found) Nothing

{-|
Find every `PDFObject` satisfying a predicate, even when deeply nested in
containers.
-}
deepFind :: (PDFObject -> Bool) -> PDFDocument -> PDFDocument
deepFind predicate = foldr walk mempty
 where
  walk :: PDFObject -> PDFDocument -> PDFDocument
  walk !object collection@(CollectionOf !objects)
    | predicate object = CollectionOf (object OS.|< objects)
    | otherwise = case object of
      (PDFTrailer eObject           ) -> walk eObject collection
      (PDFIndirectObject _ _ eObject) -> walk eObject collection
      (PDFIndirectObjectWithStream _ _ dict _) ->
        walk (PDFDictionary dict) collection
      (PDFObjectStream _ _ dict _) -> walk (PDFDictionary dict) collection
      (PDFArray      lObjects    ) -> foldr walk collection lObjects
      (PDFDictionary dict        ) -> foldr walk collection dict
      _anyOtherValue               -> collection
