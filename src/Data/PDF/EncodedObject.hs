module Data.PDF.EncodedObject
  ( EncodedObject (EncodedObject, eoObjectNumber, eoObjectLength, eoBinaryData, eoEmbeddedObjects)
  , objectCount
  , objectNumbers
  )
where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Sequence (Seq ((:<|)))

-- | An object that has been encoded
type EncodedObject :: Type
data EncodedObject = EncodedObject
  { eoObjectNumber    :: !Int -- ^ Object number
  , eoObjectLength    :: !Int -- ^ Object length (in bytes)
  , eoBinaryData      :: !ByteString   -- ^ Encoded object
  , eoEmbeddedObjects :: !(Seq Int) -- ^ Object numbers embedded in this object
  }
  deriving stock (Show)

instance Eq EncodedObject where
  (==) :: EncodedObject -> EncodedObject -> Bool
  (==) (EncodedObject objNum1 _ _ _) (EncodedObject objNum2 _ _ _) =
    objNum1 == objNum2

instance Ord EncodedObject where
  compare :: EncodedObject -> EncodedObject -> Ordering
  compare (EncodedObject objNum1 _ _ _) (EncodedObject objNum2 _ _ _) =
    compare objNum1 objNum2

{- |
Count the number of objects in an encoded object. This includes the object
itself and any embedded objects.
-}
objectCount :: EncodedObject -> Int
objectCount = (+ 1) . length . eoEmbeddedObjects

{- |
Get the object numbers of an encoded object. This includes the object itself
and any embedded objects.

Objects are returned in the order they are encountered in the encoded object.
-}
objectNumbers :: EncodedObject -> Seq Int
objectNumbers object = eoObjectNumber object :<| eoEmbeddedObjects object
