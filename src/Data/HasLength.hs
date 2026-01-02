{-|
Unified length interface for heterogeneous container types.

Provides a simple typeclass to query the length of 'String' and 'ByteString'
values uniformly via 'objectLength'.
-}
module Data.HasLength (HasLength (objectLength)) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Constraint, Type)

{-|
Typeclass for types that have an observable length.

Provides 'objectLength' to query the number of elements or bytes in a container.
-}
type HasLength :: Type -> Constraint
class HasLength a where
  {-|
  Return the length of a container.

  For 'String', this is the number of characters; for 'ByteString', the number
  of bytes.
  -}
  objectLength :: a -> Int

instance HasLength String where
  objectLength :: String -> Int
  objectLength = length

instance HasLength ByteString where
  objectLength :: ByteString -> Int
  objectLength = BS.length
