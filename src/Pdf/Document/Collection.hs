{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE InstanceSigs #-}
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

    -- * Find `PDFObject`
  , findLastValue
  , findLastObject
  ) where

import qualified Data.ByteString               as BS
import qualified Data.HashMap.Strict           as HM
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFObjectStream
                                                  )
                                                , fromPDFObject
                                                )
import Pdf.Document.Document (CollectionOf)
import           Data.Maybe                     ( isJust )
import           Control.Monad                  ( (<=<) )

-- | A collection of objects indexed by the object number
type PDFObjects = HM.HashMap Int PDFObject

-- | A collection of object offsets indexed by the object number
type ObjectOffsets = HM.HashMap Int Int

-- | An object that has been encoded
data EncodedObject = EncodedObject
  { eoObjectNumber :: Int           -- ^ Object number
  , eoObjectLength :: Int           -- ^ Object length (in bytes)
  , eoBinaryData   :: BS.ByteString -- ^ Encoded object
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

-- | Given a predicate, find the last `PDFOBject` validating this predicate
findLastObject
  :: (PDFObject -> Bool)
     -- ^ Returns `True` if the `PDFObject` satisfies a condition
  -> [PDFObject] -- ^ List of `PDFObject` to search
  -> Maybe PDFObject
     -- ^ The last `PDFObject` satisfying the predicate or Nothing
findLastObject predicate = findLastObject' Nothing
 where
  findLastObject' :: Maybe PDFObject -> [PDFObject] -> Maybe PDFObject
  findLastObject' found []                      = found
  findLastObject' found (object : otherObjects) = if predicate object
    then findLastObject' (Just object) otherObjects
    else findLastObject' found otherObjects

-- | Given a predicate, find the value of the last `PDFOBject` validating this
--   predicate
findLastValue
  :: (PDFObject -> Maybe a)
   -- ^ Returns an arbitray value from a `PDFObject` it the `PDFObject`
   --   satisfies a condition or Nothing
  -> [PDFObject] -- ^ List of `PDFObject` to search
  -> Maybe a -- ^ The last `PDFObject` satisfying the predicate or Nothing
findLastValue getValue = getValue <=< findLastObject (isJust . getValue)
