module Data.HasLength (HasLength (objectLength)) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Constraint, Type)

type HasLength :: Type -> Constraint
class HasLength a where
  objectLength :: a -> Int

instance HasLength String where
  objectLength :: String -> Int
  objectLength = length

instance HasLength ByteString where
  objectLength :: ByteString -> Int
  objectLength = BS.length
