-- |
-- This modules allows partitioning of PDF objects.
--
-- Partitioning is used when encoded a whole PDF file from PDF objects.
module PDF.Document.PDFPartition
  ( removeUnused
  , partitionDocument
  , objectToEmbed
  , objectWithContent
  ) where

import Data.Context (Contextual (ctx))
import Data.IntMap qualified as IM
import Data.Logging (Logging)
import Data.PDF.PDFDocument (PDFDocument, cFilter, deepFind, member)
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

import PDF.Document.Uncompress (uncompressDocument, uncompressObjects)
import PDF.Object.Object.Properties (hasKey)

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
    let references =
          deepFind isReference uHeads
            <> deepFind isReference uTrailers
            <> deepFind isReference (toPDFDocument uObjectsWithStream)
            <> deepFind isReference (toPDFDocument uObjectsWithoutStream)

    sayP "Removing unused objects"

    return $ PDFPartition
      { ppObjectsWithStream    = IM.filter (used references) objectsWithStream
      , ppObjectsWithoutStream = IM.filter (used references) objectsWithoutStream
      , ppHeads                = heads
      , ppTrailers             = trailers
      }
 where
  isNotLinearized :: PDFObject -> Bool
  isNotLinearized = not . hasKey "Linearized"

  isReferenced :: PDFDocument -> PDFObject -> Bool
  isReferenced refs (PDFIndirectObject num gen _) =
    PDFReference num gen `member` refs
  isReferenced refs (PDFIndirectObjectWithStream num gen _ _) =
    PDFReference num gen `member` refs
  isReferenced refs (PDFObjectStream num gen _ _) =
    PDFReference num gen `member` refs
  isReferenced _anyRefs _anyOtherObject = True

  used :: PDFDocument -> PDFObject -> Bool
  used refs object = isNotLinearized object && isReferenced refs object

{- |
Determines if a PDF object should be embedded in an object stream (ObjStm).

Returns `True` for indirect objects that do not have a stream.
-}
objectToEmbed :: PDFObject -> Bool
objectToEmbed object = isIndirect object && not (hasStream object)

{- |
Checks if a PDF object contains content (i.e., it has a stream but is not a
trailer).

Returns `True` if the object has a stream and is not a trailer.
-}
objectWithContent :: PDFObject -> Bool
objectWithContent object = hasStream object && not (isTrailer object)

{- |
Partitions a collection of PDF objects (`PDFDocument`) into a `PDFPartition`,
separating objects with streams, objects without streams, header objects, and
trailers.
-}
partitionDocument :: PDFDocument -> PDFPartition
partitionDocument objs =
  PDFPartition
    { ppObjectsWithStream    = fromPDFDocument $ cFilter objectWithContent objs
    , ppObjectsWithoutStream = fromPDFDocument $ cFilter objectToEmbed objs
    , ppHeads                = cFilter isHeader objs
    , ppTrailers             = cFilter isTrailer objs
    }
