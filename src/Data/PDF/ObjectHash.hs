module Data.PDF.ObjectHash
  ( ObjectHash(ObjectHash, ohObjectNumber, ohHash)
  , objectHash
  ) where

import Data.Kind (Type)
import Data.PDF.PDFObject (PDFObject (PDFIndirectObjectWithStream))
import Data.Text qualified as T

import PDF.Object.Signature (streamHash)

{-|
A hash representation of a PDF object with a stream.

Stores the object number and its textual hash representation. Equality and
ordering consider only the hash; the object number is ignored to allow
de-duplication by content.
-}
type ObjectHash :: Type
data ObjectHash = ObjectHash
  { ohObjectNumber :: !Int
  , ohHash         :: !T.Text
  }

instance Eq ObjectHash where
  (==) :: ObjectHash -> ObjectHash -> Bool
  (ObjectHash _ hash1) == (ObjectHash _ hash2) = hash1 == hash2

instance Ord ObjectHash where
  compare :: ObjectHash -> ObjectHash -> Ordering
  (ObjectHash _ hash1) `compare` (ObjectHash _ hash2) = hash1 `compare` hash2

instance Semigroup ObjectHash where
  (<>) :: ObjectHash -> ObjectHash -> ObjectHash
  (<>) _anyObjectHash newObject = newObject

instance Monoid ObjectHash where
  mempty :: ObjectHash
  mempty = ObjectHash 0 ""

{-
Attempt to compute an 'ObjectHash' for a 'PDFObject'.

Returns 'Just' when the object is an indirect object with a stream, using
'streamHash' to compute the hash; otherwise returns 'Nothing'.
-}
objectHash :: PDFObject -> Maybe ObjectHash
objectHash object@(PDFIndirectObjectWithStream number _ _ _) =
  ObjectHash number . T.pack . show <$> streamHash object
objectHash _anyOtherObject = Nothing
