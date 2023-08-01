{- |
This module manipulates collections of `PDFObject`.

These functions are meant to be used when optimizing/encoding a PDF.
-}
module Pdf.Document.Collection
  ( -- * Encoding of object collections
    encodeObject
  , PDFObjects
  , EncodedObject(EncodedObject, eoObjectNumber, eoObjectLength, eoBinaryData)
  , EncodedObjects
  , ObjectOffsets
  ) where

import qualified Data.ByteString               as BS
import qualified Data.IntMap.Strict            as IM
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFObjectStream
                                                  )
                                                , fromPDFObject
                                                )
import           Pdf.Document.Document          ( CollectionOf )

-- | A collection of objects indexed by the object number
type PDFObjects = IM.IntMap PDFObject

-- | A collection of object offsets indexed by the object number
type ObjectOffsets = IM.IntMap Int

-- | An object that has been encoded
data EncodedObject = EncodedObject
  { eoObjectNumber :: !Int           -- ^ Object number
  , eoObjectLength :: !Int           -- ^ Object length (in bytes)
  , eoBinaryData   :: !BS.ByteString -- ^ Encoded object
  }
  deriving stock (Eq, Show)

-- | A collection of encoded objects with no duplicate
type EncodedObjects = CollectionOf EncodedObject

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
