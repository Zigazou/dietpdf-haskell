{-|
Merge vector streams from PDF objects.

Provides utilities for extracting and combining multiple PDF streams,
particularly for vector graphics content that may be split across multiple
objects or arrays.
-}
module PDF.Document.MergeVectorStream
  ( mergeVectorStream
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.Logging (Logging)
import Data.PDF.PDFObject
    ( PDFObject (PDFArray, PDFIndirectObject, PDFIndirectObjectWithStream, PDFNumber, PDFReference)
    )
import Data.PDF.PDFWork (PDFWork, getReference)

import PDF.Processing.Unfilter (unfilter)

import Util.Ascii (asciiSPACE)
import Util.Dictionary (mkDictionary)

{-|
Recursively extract and concatenate streams from a PDF object.

Handles various PDF object types: follows references, extracts stream content
from indirect objects, recursively processes arrays by concatenating all streams
with space separators, and returns empty for non-stream objects.
-}
allStreams :: Logging m => PDFObject -> PDFWork m ByteString
allStreams reference@PDFReference{} = getReference reference >>= allStreams

allStreams object@PDFIndirectObjectWithStream{} = unfilter object <&> \case
  PDFIndirectObjectWithStream _major _minor _dict stream -> stream
  _anythingElse                                          -> mempty

allStreams (PDFIndirectObject _major _minor object) = allStreams object

allStreams (PDFArray objects) =
  mapM allStreams objects <&> foldl ((. BS.cons asciiSPACE) . BS.append) mempty

allStreams _anyOtherObject = return mempty

{-|
Merge a vector stream or stream array into a single PDF stream object.

Extracts and concatenates all streams from the input object (which may be a
reference, array, or direct stream), calculates the resulting stream length, and
returns a new 'PDFIndirectObjectWithStream' containing the merged content.
-}
mergeVectorStream :: Logging m => PDFObject -> PDFWork m PDFObject
mergeVectorStream object = do
  stream <- allStreams object
  let streamLength = fromIntegral (BS.length stream) :: Double
  return $ PDFIndirectObjectWithStream
            0
            0
            (mkDictionary [("Length", PDFNumber streamLength)])
            stream
