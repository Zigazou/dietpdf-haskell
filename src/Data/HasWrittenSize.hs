{-|
Unified written size interface for heterogeneous container types.

Provides a simple typeclass to query the written size of 'ByteString' values
uniformly via 'writtenSize'.
-}
module Data.HasWrittenSize (HasWrittenSize (writtenSize)) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Constraint, Type)

{-|
Typeclass for types that may eventually be written in a file.

Provides 'writtenSize' to query how many bytes will be written in a file.
-}
type HasWrittenSize :: Type -> Constraint
class HasWrittenSize a where
  {-|
  Return the written size of a container.

  For 'ByteString', this is the number of bytes.
  -}
  writtenSize :: a -> Int

instance HasWrittenSize ByteString where
  writtenSize :: ByteString -> Int
  writtenSize = BS.length
