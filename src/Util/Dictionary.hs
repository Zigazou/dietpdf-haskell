{-|
Utilities for working with byte-string keyed dictionaries.

Defines a `Dictionary` type alias keyed by `ByteString` and helpers to
construct, query, alter, and search dictionaries. Intended for use with
PDF object dictionaries and similar structures.
-}
module Util.Dictionary
  ( Dictionary
  , mkDictionary
  , mkEmptyDictionary
  , dictHasKey
  , dictAlter
  , findFirst
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Map (Map, foldlWithKey)
import Data.Map qualified as Map
import Data.Maybe (isJust)

{-|
Convenience alias for a `Map` from `ByteString` to values of type `a`.
-}
type Dictionary :: Type -> Type
type Dictionary a = Map ByteString a

{-| Return an empty `Dictionary`. -}
mkEmptyDictionary :: Dictionary a
mkEmptyDictionary = mempty

{-| Create a dictionary from an association list of key-value pairs. -}
mkDictionary :: [(ByteString, a)] -> Dictionary a
mkDictionary = Map.fromList

{-|
Determine if a key is in a dictionary from a `PDFObject`.

If the `PDFObject` has no dictionary, it returns `False`.
-}
dictHasKey
  :: ByteString
  -> Dictionary a
  -> Bool
dictHasKey = (isJust .) . Map.lookup

{-|
Alter the value at a given key.

Behavior:

* `Just v` inserts or replaces with `v`.
* `Nothing` removes the key if present.
-}
dictAlter
  :: ByteString
  -> Maybe a
  -> Dictionary a
  -> Dictionary a
dictAlter key value dict = let replaceValue _anyValue = value
                           in Map.alter replaceValue key dict

{-|
Find the first key whose associated value satisfies a predicate.

Returns `Nothing` when no entry matches.
-}
findFirst :: (a -> Bool) -> Dictionary a -> Maybe ByteString
findFirst predicate = foldlWithKey (validatePredicate predicate) Nothing
  where
    validatePredicate
      :: (a -> Bool)
      -> Maybe ByteString
      -> ByteString
      -> a
      -> Maybe ByteString
    validatePredicate predicate' acc key value | predicate' value = Just key
                                               | otherwise        = acc
