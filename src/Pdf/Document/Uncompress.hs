module Pdf.Document.Uncompress
  ( uncompressObjects
  , uncompressDocument
  ) where

import Data.Functor ((<&>))

import Pdf.Document.Collection (PDFObjects)
import Pdf.Document.Document (PDFDocument, fromList, toList)
import Pdf.Document.ObjectStream (explodeDocument, explodeObjects)
import Pdf.Object.Unfilter (unfilter)

import Util.Logging (Logging, sayF)
import Util.UnifiedError (FallibleT)

{- |
Uncompress all `PDFObject` contained in a `PDFDObjects`.

Objects embedded in object streams are extracted and the object stream is
removed.

If a `PDFObject` cannot be uncompressed (meaning its processing generated an
error), the object is left as is. Thus this function may leave object
uncompressed.
-}
uncompressObjects :: Logging m => PDFObjects -> FallibleT m PDFObjects
uncompressObjects pdf = do
  sayF "  - Extracting objects from object streams"
  objects <- explodeObjects pdf

  sayF "  - Unfiltering all objects"
  mapM unfilter objects

{- |
Uncompress all `PDFObject` contained in a `PDFDocument`.

Objects embedded in object streams are extracted and the object stream is
removed.

If a `PDFObject` cannot be uncompressed (meaning its processing generated an
error), the object is left as is. Thus this function may leave object
uncompressed.
-}
uncompressDocument :: Logging m => PDFDocument -> FallibleT m PDFDocument
uncompressDocument pdf = do
  sayF "  - Extracting objects from object streams"
  objects <- explodeDocument pdf <&> toList

  sayF "  - Unfiltering all objects"
  fromList <$> mapM unfilter objects
