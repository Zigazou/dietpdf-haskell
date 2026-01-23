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
import Data.Maybe (mapMaybe)
import Data.PDF.ObjectHash (ObjectHash (ObjectHash), objectHash)
import Data.PDF.PDFDocument (PDFDocument, toList)
import Data.PDF.PDFWork (PDFWork, evalPDFWork, sayP, withContext)
import Data.Set (Set, empty, insert, member)
import Data.Text qualified as T

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
