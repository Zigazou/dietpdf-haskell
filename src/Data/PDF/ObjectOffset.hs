{-|
Object offset in a PDF file
-}
module Data.PDF.ObjectOffset
  ( ObjectOffset (DirectOffset, InObjectStream, FreeEntry),
    getOffsetValue,
    ooObjectNumber,
  )
where

import Data.Kind (Type)

{-|
Representation of a PDF object offset.
-}
type ObjectOffset :: Type
data ObjectOffset
  = DirectOffset !Int !Int -- ^ Direct object offset (object number, offset)
  | InObjectStream !Int !Int !Int -- ^ Embedded object offset (object number, object stream, index)
  | FreeEntry !Int -- ^ (object number)
  deriving stock (Show)

instance Eq ObjectOffset where
  (==) :: ObjectOffset -> ObjectOffset -> Bool
  (==) oo1 oo2 = ooObjectNumber oo1 == ooObjectNumber oo2

instance Ord ObjectOffset where
  compare :: ObjectOffset -> ObjectOffset -> Ordering
  compare oo1 oo2 = compare (ooObjectNumber oo1) (ooObjectNumber oo2)

{-|
Get the object number of an object offset. 
-}
ooObjectNumber :: ObjectOffset -> Int
ooObjectNumber (DirectOffset objectNumber _)     = objectNumber
ooObjectNumber (InObjectStream objectNumber _ _) = objectNumber
ooObjectNumber (FreeEntry objectNumber)          = objectNumber

{-|
Get the offset value of an object offset.

This is the offset of the object in the PDF file or the index of the object in
an object stream.

For free entries, the offset value is 0.
-}
getOffsetValue :: ObjectOffset -> Int
getOffsetValue (DirectOffset _objectNumber offset)                 = offset
getOffsetValue (InObjectStream _objectNumber _objectStream offset) = offset
getOffsetValue (FreeEntry _objectNumber)                           = 0
