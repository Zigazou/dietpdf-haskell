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

allStreams :: Logging m => PDFObject -> PDFWork m ByteString
allStreams reference@PDFReference{} = getReference reference >>= allStreams

allStreams object@PDFIndirectObjectWithStream{} = unfilter object <&> \case
  PDFIndirectObjectWithStream _major _minor _dict stream -> stream
  _anythingElse                                          -> mempty

allStreams (PDFIndirectObject _major _minor object) = allStreams object

allStreams (PDFArray objects) =
  mapM allStreams objects <&> foldl ((. BS.cons asciiSPACE) . BS.append) mempty

allStreams _anyOtherObject = return mempty

mergeVectorStream :: Logging m => PDFObject -> PDFWork m PDFObject
mergeVectorStream object = do
  stream <- allStreams object
  let streamLength = fromIntegral (BS.length stream) :: Double
  return $ PDFIndirectObjectWithStream
            0
            0
            (mkDictionary [("Length", PDFNumber streamLength)])
            stream
