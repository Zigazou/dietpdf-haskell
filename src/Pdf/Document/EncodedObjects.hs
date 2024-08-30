module Pdf.Document.EncodedObjects
  ( EncodedObjects,
  )
where

import Data.IntMap.Strict qualified as IM
import Data.Kind (Type)

import Pdf.Document.EncodedObject (EncodedObject)

-- | A collection of encoded objects with no duplicate
type EncodedObjects :: Type
type EncodedObjects = IM.IntMap EncodedObject
