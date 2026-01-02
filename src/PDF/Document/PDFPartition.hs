{-|
Partition PDF objects by type and content.

Provides utilities for organizing PDF objects into separate categories (with
streams, without streams, headers, trailers) for efficient encoding and
deduplication. Includes dead-object elimination and object stream embedding
logic.
-}
module PDF.Document.PDFPartition
  ( removeUnused
  , partitionDocument
  , objectToEmbed
  , objectWithContent
  ) where

import Data.Context (Contextual (ctx))
import Data.Foldable (toList)
import Data.IntMap qualified as IM
import Data.Logging (Logging)
import Data.PDF.PDFDocument (PDFDocument, cFilter, deepFind)
import Data.PDF.PDFObject
  ( PDFObject (PDFIndirectObject, PDFIndirectObjectWithStream, PDFObjectStream, PDFReference)
  , hasStream
  , isHeader
  , isIndirect
  , isReference
  , isTrailer
  )
import Data.PDF.PDFObjects (fromPDFDocument, toPDFDocument)
import Data.PDF.PDFPartition
  ( PDFPartition (PDFPartition, ppHeads, ppObjectsWithStream, ppObjectsWithoutStream, ppTrailers)
  )
import Data.PDF.PDFWork (PDFWork, sayP, withContext)
import Data.Set (Set)
import Data.Set qualified as Set

import PDF.Document.Uncompress (uncompressDocument, uncompressObjects)
import PDF.Object.Object.Properties (hasKey)

{-|
Remove unused objects from a PDF partition.

Uncompresses all objects in the partition, traces all references starting from
headers and trailers, and removes any objects not reachable through references.
Returns the cleaned partition with only referenced objects. Linearized document
markers are also removed during this process.
-}
removeUnused :: Logging m => PDFPartition -> PDFWork m PDFPartition
removeUnused (PDFPartition objectsWithStream objectsWithoutStream heads trailers) =
  withContext (ctx ("removeUnused" :: String)) $ do
    sayP "Uncompressing objects with stream"
    uObjectsWithStream <- uncompressObjects objectsWithStream
    sayP "Uncompressing objects without stream"
    uObjectsWithoutStream <- uncompressObjects objectsWithoutStream
    sayP "Uncompressing head objects"
    uHeads <- uncompressDocument heads
    sayP "Uncompressing trailer objects"
    uTrailers <- uncompressDocument trailers

    sayP "Locating all references"
    let
      startReferences :: Set PDFObject
      startReferences =
        (Set.fromList . toList) (  deepFind isReference uHeads
                                <> deepFind isReference uTrailers
                                )

      otherReferences :: Set PDFObject
      otherReferences =
        (Set.fromList . toList)
          (  deepFind isReference (toPDFDocument uObjectsWithStream)
          <> deepFind isReference (toPDFDocument uObjectsWithoutStream)
          )

      references = findUsedReferences startReferences otherReferences

    sayP "Removing unused objects"

    return $ PDFPartition
      { ppObjectsWithStream    = IM.filter (used references) objectsWithStream
      , ppObjectsWithoutStream = IM.filter (used references) objectsWithoutStream
      , ppHeads                = heads
      , ppTrailers             = trailers
      }
 where
  findUsedReferences :: Set PDFObject -> Set PDFObject -> Set PDFObject
  findUsedReferences startRefs refs =
    let usedRefs = Set.filter (isReferenced startRefs) refs
    in if usedRefs /= mempty
        then findUsedReferences (Set.union startRefs usedRefs)
                                (Set.difference refs usedRefs)
        else startRefs

  isNotLinearized :: PDFObject -> Bool
  isNotLinearized = not . hasKey "Linearized"

  isReferenced :: Set PDFObject -> PDFObject -> Bool
  isReferenced refs (PDFIndirectObject num gen _) =
    PDFReference num gen `Set.member` refs
  isReferenced refs (PDFIndirectObjectWithStream num gen _ _) =
    PDFReference num gen `Set.member` refs
  isReferenced refs (PDFObjectStream num gen _ _) =
    PDFReference num gen `Set.member` refs
  isReferenced _anyRefs _anyOtherObject = True

  used :: Set PDFObject -> PDFObject -> Bool
  used refs object = isNotLinearized object && isReferenced refs object

{-|
Test whether a PDF object can be embedded in an object stream.

Returns 'True' for indirect objects that do not contain streams. These objects
are suitable for inclusion in ObjStm (object stream) containers to reduce file
size.
-}
objectToEmbed :: PDFObject -> Bool
objectToEmbed object = isIndirect object && not (hasStream object)

{-|
Test whether a PDF object contains stream content.

Returns 'True' for objects that have a stream but are not trailer objects. These
are typically page or graphics stream objects that require special handling.
-}
objectWithContent :: PDFObject -> Bool
objectWithContent object = hasStream object && not (isTrailer object)

{-|
Partition a PDF document into typed object categories.

Separates objects into four categories based on their type and content:

* Objects with streams (pages, graphics): stored separately
* Objects without streams: candidates for object stream embedding
* Header objects: document catalog and related metadata
* Trailer objects: PDF trailer dictionary

Returns a 'PDFPartition' organizing objects for efficient encoding.
-}
partitionDocument :: PDFDocument -> PDFPartition
partitionDocument objs =
  PDFPartition
    { ppObjectsWithStream    = fromPDFDocument $ cFilter objectWithContent objs
    , ppObjectsWithoutStream = fromPDFDocument $ cFilter objectToEmbed objs
    , ppHeads                = cFilter isHeader objs
    , ppTrailers             = cFilter isTrailer objs
    }
