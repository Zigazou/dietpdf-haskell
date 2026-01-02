{-|
Computes and logs hashes of PDF stream objects, de-duplicating repeated
streams across a document.

This module exposes 'objectHashes', which traverses a 'PDFDocument', derives a
hash for each indirect object with a stream, and prints the hash alongside the
object number. Duplicate hashes are detected and marked. Hashing is provided by
'streamHash', and logging uses the 'PDFWork' context via 'sayP'.
-}
module Command.Hash
  ( objectHashes
  ) where

import Data.Context (ctx)
import Data.Fallible (FallibleT)
import Data.Foldable (foldrM)
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.PDF.PDFDocument (PDFDocument, toList)
import Data.PDF.PDFObject (PDFObject (PDFIndirectObjectWithStream))
import Data.PDF.PDFWork (PDFWork, evalPDFWork, sayP, withContext)
import Data.Set (Set, empty, insert, member)
import Data.Text qualified as T

import PDF.Object.Signature (streamHash)

{-|
Hash record for an indirect object with stream.

Stores the object number and its textual hash representation. Equality and
ordering consider only the hash; the object number is ignored to allow
de-duplication by content.
-}
type ObjectHash :: Type
data ObjectHash = ObjectHash !Int !T.Text

instance Eq ObjectHash where
  (==) :: ObjectHash -> ObjectHash -> Bool
  (ObjectHash _ hash1) == (ObjectHash _ hash2) = hash1 == hash2

instance Ord ObjectHash where
  compare :: ObjectHash -> ObjectHash -> Ordering
  (ObjectHash _ hash1) `compare` (ObjectHash _ hash2) = hash1 `compare` hash2

{-|
Compute and print hashes for all stream objects in the given document.

Behavior:

* Iterates over the 'PDFDocument' and attempts to build 'ObjectHash' values
  using 'streamHash'.
* Prints each hash with its object number. When a hash repeats, marks it as
  "duplicate!" and does not add it to the set again.
* Runs inside 'PDFWork' via 'evalPDFWork', using 'sayP' for logging and
  'withContext' to tag log entries.
-}
objectHashes :: PDFDocument -> FallibleT IO ()
objectHashes document = evalPDFWork $ do
  let
    hashes :: [ObjectHash]
    hashes = mapMaybe objectHash (toList document)

  _ <- foldrM printHashForObject empty hashes
  return ()
 where
  {-
  Attempt to compute an 'ObjectHash' for a 'PDFObject'.

  Returns 'Just' when the object is an indirect object with a stream, using
  'streamHash' to compute the hash; otherwise returns 'Nothing'.
  -}
  objectHash :: PDFObject -> Maybe ObjectHash
  objectHash object@(PDFIndirectObjectWithStream number _ _ _) =
    ObjectHash number . T.pack . show <$> streamHash object
  objectHash _anyOtherObject = Nothing

  {-
  Print a single 'ObjectHash', updating the set of seen hashes.

  If the hash has already been observed, logs the hash, object number, and the
  "duplicate!" marker. Otherwise logs the hash and number and inserts it into
  the set. Uses a logging context label "objecthashes".
  -}
  printHashForObject
    :: ObjectHash -> Set ObjectHash -> PDFWork IO (Set ObjectHash)
  printHashForObject oHash@(ObjectHash number hash) hashFound =
    withContext (ctx ("objecthashes" :: String)) $ do
      if member oHash hashFound
        then do
          sayP $ T.concat [hash, "\t", T.pack (show number), " duplicate!"]
          return hashFound
        else do
          sayP $ T.concat [hash, "\t", T.pack (show number)]
          return (insert oHash hashFound)
