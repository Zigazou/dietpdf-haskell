{-|
This module handles dictionary at the lowest level.
-}
module Util.Dictionary
  ( Dictionary
  , mkDictionary
  , mkEmptyDictionary
  , dictHasKey
  ) where

import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)

{- |
A `Dictionary` is a handy type.

It is a `Map` of object of type a indexed by `ByteString`.
-}
type Dictionary :: Type -> Type
type Dictionary a = Map.Map BS.ByteString a

-- | Returns an empty `Dictionary`
mkEmptyDictionary :: Dictionary a
mkEmptyDictionary = Map.empty

-- | Create a dictionary from a list of key-value couples.
mkDictionary :: [(BS.ByteString, a)] -> Dictionary a
mkDictionary = Map.fromList

{- |
Determine if a key is in a dictionary from a `PDFObject`.

If the `PDFObject` has no dictionary, it returns `False`.
-}
dictHasKey
  :: BS.ByteString -- ^ The key to search for
  -> Dictionary a -- ^ The `Dictionary` to search in
  -> Bool
dictHasKey = (isJust .) . Map.lookup
