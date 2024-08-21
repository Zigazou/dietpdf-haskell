{- |
This module manipulates collections of `PDFObject`.

These functions are meant to be used when optimizing/encoding a PDF.
-}
module Pdf.Document.Collection
  ( -- * Encoding of object collections
    encodeObject
  , PDFObjects
  , toPDFDocument
  , fromPDFDocument
  , findLast
  , EncodedObject(EncodedObject, eoObjectNumber, eoObjectLength, eoBinaryData)
  , EncodedObjects
  , ObjectOffsets
  ) where

import           Data.Kind                      ( Type )
import qualified Data.ByteString               as BS
import qualified Data.IntMap.Strict            as IM
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFObjectStream
                                                  , PDFIndirectObjectWithGraphics
                                                  , PDFXRefStream
                                                  )
                                                , fromPDFObject
                                                )
import           Pdf.Document.Document          ( PDFDocument
                                                , fromList
                                                , toList
                                                )
import           Data.List                      ( find )

-- | A collection of objects indexed by the object number
type PDFObjects :: Type
type PDFObjects = IM.IntMap PDFObject

toPDFDocument :: PDFObjects -> PDFDocument
toPDFDocument = fromList . fmap snd . IM.toAscList

fromPDFDocument :: PDFDocument -> PDFObjects
fromPDFDocument = IM.fromList . fmap createCouple . toList
 where
  createCouple :: PDFObject -> (Int, PDFObject)
  createCouple object@(PDFIndirectObject number _ _) = (number, object)
  createCouple object@(PDFIndirectObjectWithGraphics number _ _ _) =
    (number, object)
  createCouple object@(PDFIndirectObjectWithStream number _ _ _) =
    (number, object)
  createCouple object@(PDFXRefStream number _ _ _) = (number, object)
  createCouple object                              = (0, object)

{- |
Find last value in a `CollectionOf` satisfying a predicate.

If the predicate is never satisfied, the function returns `Nothing`.
-}
findLast :: (PDFObject -> Bool) -> PDFObjects -> Maybe PDFObject
findLast p = find p . fmap snd . IM.toDescList

-- | A collection of object offsets indexed by the object number
type ObjectOffsets :: Type
type ObjectOffsets = IM.IntMap Int

-- | An object that has been encoded
type EncodedObject :: Type
data EncodedObject = EncodedObject
  { eoObjectNumber :: !Int           -- ^ Object number
  , eoObjectLength :: !Int           -- ^ Object length (in bytes)
  , eoBinaryData   :: !BS.ByteString -- ^ Encoded object
  }
  deriving stock (Eq, Show)

-- | A collection of encoded objects with no duplicate
type EncodedObjects :: Type
type EncodedObjects = IM.IntMap EncodedObject

instance Ord EncodedObject where
  compare :: EncodedObject -> EncodedObject -> Ordering
  compare (EncodedObject objNum1 _ _) (EncodedObject objNum2 _ _) =
    compare objNum1 objNum2

-- | Encodes a PDF object and keep track of its number and length.
encodeObject :: PDFObject -> EncodedObject
encodeObject object@(PDFIndirectObject number _ _) = EncodedObject
  number
  (BS.length bytes)
  bytes
  where bytes = fromPDFObject object
encodeObject object@(PDFIndirectObjectWithStream number _ _ _) = EncodedObject
  number
  (BS.length bytes)
  bytes
  where bytes = fromPDFObject object
encodeObject object@(PDFObjectStream number _ _ _) = EncodedObject
  number
  (BS.length bytes)
  bytes
  where bytes = fromPDFObject object
encodeObject object = EncodedObject 0 (BS.length bytes) bytes
  where bytes = fromPDFObject object
