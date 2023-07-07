{-# LANGUAGE StrictData #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}
module Pdf.Document.Document
  ( PDFDocument
  , CollectionOf(CollectionOf)
  , singleton
  , fromList
  , toList
  , cSize
  , cCons
  , cMap
  , cfMap
  , cFilter
  , lMap
  , dSepBy1
  , dSepBy
  , deepFind
  , findLast
  , clean
  ) where

import           Control.Applicative            ( (<|>)
                                                , Alternative
                                                , liftA2
                                                )
import           Data.Foldable                  ( foldl' )
import qualified Data.Set.Ordered              as OS
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFArray
                                                  , PDFComment
                                                  , PDFDictionary
                                                  , PDFEndOfFile
                                                  , PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFObjectStream
                                                  , PDFStartXRef
                                                  , PDFTrailer
                                                  , PDFVersion
                                                  , PDFXRef
                                                  , PDFIndirectObjectWithGraphics
                                                  )
                                                )
import           Util.UnifiedError              ( FallibleT )

{- |
This is a simple trick to allow `PDFDocument` to be foldable because it is not
of kind '* -> *' as required by the Foldable class.
-}
newtype CollectionOf a = CollectionOf (OS.OSet a) deriving stock (Eq, Show)

{- |
A `PDFDocument` is a collection of `PDFObject` keeping the insertion order and
having no duplicates.

Every function working on `CollectionOf` works on `PDFDocument`.
-}
type PDFDocument = CollectionOf PDFObject

instance Foldable CollectionOf where
  foldMap :: Monoid m => (a -> m) -> CollectionOf a -> m
  foldMap f (CollectionOf dict) = foldMap f dict

-- | The `<>` operator does a concatenation when used with `CollectionOf`.
instance Ord a => Semigroup (CollectionOf a) where
  (<>) :: CollectionOf a -> CollectionOf a -> CollectionOf a
  (<>) (CollectionOf x) (CollectionOf y) = CollectionOf (x OS.<>| y)

instance Ord a => Monoid (CollectionOf a) where
  mempty :: CollectionOf a
  mempty = CollectionOf OS.empty

cSize :: CollectionOf a -> Int
cSize (CollectionOf os) = OS.size os

{- |
A `map` function for objects of type `CollectionOf`.

The destination type must be an instance of `Ord`.
-}
cMap :: Ord b => (a -> b) -> CollectionOf a -> CollectionOf b
cMap f = foldr (cCons . f) mempty

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
  accumulate transform a fc = do
    collection <- fc
    result     <- transform a
    return (cCons result collection)

{- |
Equivalent to the `filter` function which works on `List` except this one
works on `CollectionOf`.
-}
cFilter :: Ord a => (a -> Bool) -> CollectionOf a -> CollectionOf a
cFilter f (CollectionOf set) = CollectionOf $ OS.filter f set

{- |
A `map` function for objects of type `CollectionOf` which converts to a `List`.

This allows to get rid of the `Ord` constraint of cMap.
-}
lMap :: (a -> b) -> CollectionOf a -> [b]
lMap f (CollectionOf objects) = (fmap f . OS.toAscList) objects

{- |
Create a `PDFDocument` with only one `PDFObject`.
-}
singleton :: PDFObject -> PDFDocument
singleton = CollectionOf . OS.singleton

{- |
Convert a list of `PDFObject` to a `PDFDocument`.

Only the last `PDFVersion`, `PDFXRef`, `PDFTrailer` are kept.
Only the last indirect object with a specific number is kept.
-}
fromList :: Ord a => [a] -> CollectionOf a
fromList = CollectionOf . foldl' (OS.>|) OS.empty

{- |
Convert a `CollectionOf` to a `List`.
-}
toList :: CollectionOf a -> [a]
toList (CollectionOf objects) = OS.toAscList objects

{- |
The cons function for `CollectionOf` type.
-}
cCons :: Ord a => a -> CollectionOf a -> CollectionOf a
cCons object (CollectionOf objects) = CollectionOf (object OS.|< objects)

{- |
The `sepBy1` function for `CollectionOf` (`sepBy1` only generates `List`).
-}
dSepBy1 :: (Alternative f, Ord a) => f a -> f s -> f (CollectionOf a)
dSepBy1 p s = go where go = liftA2 cCons p ((s *> go) <|> pure mempty)

{- |
The `sepBy` function for `CollectionOf` (`sepBy` only generates `List`).
-}
dSepBy :: (Alternative f, Ord a) => f a -> f s -> f (CollectionOf a)
dSepBy p s =
  liftA2 cCons p ((s *> dSepBy1 p s) <|> pure mempty) <|> pure mempty

{- |
Find last value in a `CollectionOf` satisfying a predicate.

If the predicate is never satisfied, the function returns `Nothing`.
-}
findLast :: (a -> Bool) -> CollectionOf a -> Maybe a
findLast p =
  foldr (\object found -> if p object then Just object else found) Nothing

{- |
Find every `PDFObject` satisfiying a predicate, even when deeply nested in
containers.
-}
deepFind :: (PDFObject -> Bool) -> PDFDocument -> PDFDocument
deepFind p = foldr walk mempty
 where
  walk :: PDFObject -> PDFDocument -> PDFDocument
  walk object collection@(CollectionOf objects)
    | p object = CollectionOf (object OS.|< objects)
    | otherwise = case object of
      (PDFTrailer eObject           ) -> walk eObject collection
      (PDFIndirectObject _ _ eObject) -> walk eObject collection
      (PDFIndirectObjectWithStream _ _ dict _) ->
        walk (PDFDictionary dict) collection
      (PDFObjectStream _ _ dict _) -> walk (PDFDictionary dict) collection
      (PDFArray      lObjects    ) -> collection <> foldr walk mempty lObjects
      (PDFDictionary dict        ) -> collection <> foldr walk mempty dict
      _anyOtherValue               -> collection

{- |
Remove objects that are not top-level from the PDFDocument.

Any object other than the following is filtered out:

- `PDFEndOfFile`
- `PDFIndirectObject`
- `PDFIndirectObjectWithStream`
- `PDFObjectStream`
- `PDFTrailer`
- `PDFVersion`
- `PDFXRef`

>>> clean (fromList [PDFName "a"])
fromList []

>>> clean (fromList [PDFVersion "1.2", PDFVersion "1.5"])
fromList [PDFVersion "1.5"]

>>> clean (fromList [PDFIndirectObject 1 0 PDFNull, PDFIndirectObject 1 0 (PDFName "a")])
fromList [PDFIndirectObject 1 0 (PDFName "a")]
-}
clean :: PDFDocument -> PDFDocument
clean = cFilter topLevel
 where
  topLevel :: PDFObject -> Bool
  topLevel (PDFComment _)                  = True
  topLevel PDFEndOfFile                    = True
  topLevel PDFIndirectObject{}             = True
  topLevel PDFIndirectObjectWithStream{}   = True
  topLevel PDFIndirectObjectWithGraphics{} = True
  topLevel PDFObjectStream{}               = True
  topLevel PDFTrailer{}                    = True
  topLevel PDFVersion{}                    = True
  topLevel PDFXRef{}                       = True
  topLevel PDFStartXRef{}                  = True
  topLevel _anyOtherObject                 = False
