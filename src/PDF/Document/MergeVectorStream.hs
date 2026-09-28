{-|
Merge vector streams from PDF objects.

Provides utilities for extracting and combining multiple PDF streams,
particularly for vector graphics content that may be split across multiple
objects or arrays.
-}
module PDF.Document.MergeVectorStream
  ( mergeVectorStream
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.List (intersperse)
import Data.Logging (Logging)
import Data.PDF.PDFObject
  ( PDFObject (PDFArray, PDFIndirectObject, PDFIndirectObjectWithStream, PDFNumber, PDFReference)
  )
import Data.PDF.PDFWork (PDFWork, getReference)
import Data.Set qualified as Set

import PDF.Processing.Unfilter (unfilter)

import Util.Ascii (asciiSPACE)
import Util.Dictionary (mkDictionary)

{-|
Recursively extract and concatenate streams from a PDF object.

Handles various PDF object types: follows references, extracts stream content
from indirect objects, recursively processes arrays by concatenating all streams
with space separators, and returns empty for non-stream objects.
-}
allStreams :: Logging m => PDFObject -> PDFWork m BB.Builder
allStreams = allStreamsSeen Set.empty

-- Track references on the current traversal path. This prevents malformed
-- cyclic /Contents references from recursing forever while still allowing the
-- same stream to appear more than once in a Contents array.
allStreamsSeen :: Logging m => Set.Set Int -> PDFObject -> PDFWork m BB.Builder
allStreamsSeen seen reference@(PDFReference objectNumber _) =
  if Set.member objectNumber seen
    then
      return mempty
    else
      getReference reference >>= allStreamsSeen (Set.insert objectNumber seen)

allStreamsSeen _seen object@PDFIndirectObjectWithStream{} =
  unfilter object <&> \case
    PDFIndirectObjectWithStream _major _minor _dict stream
      -> BB.byteString stream

    _anythingElse
      -> mempty

allStreamsSeen seen (PDFIndirectObject _major _minor object)
  = allStreamsSeen seen object

allStreamsSeen seen (PDFArray objects) =
  mapM (allStreamsSeen seen) objects
    <&> mconcat
      . intersperse (BB.word8 asciiSPACE)
      . toList

allStreamsSeen _seen _anyOtherObject = return mempty

{-|
Merge a vector stream or stream array into a single PDF stream object.

Extracts and concatenates all streams from the input object (which may be a
reference, array, or direct stream), calculates the resulting stream length, and
returns a new 'PDFIndirectObjectWithStream' containing the merged content.
-}
mergeVectorStream :: Logging m => PDFObject -> PDFWork m PDFObject
mergeVectorStream object = do
  stream <- LBS.toStrict . BB.toLazyByteString <$> allStreams object

  let
    streamLength :: Double
    streamLength = fromIntegral (BS.length stream) :: Double

  return $ PDFIndirectObjectWithStream
            0
            0
            (mkDictionary [("Length", PDFNumber streamLength)])
            stream
