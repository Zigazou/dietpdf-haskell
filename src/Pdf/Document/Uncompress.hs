module Pdf.Document.Uncompress
  ( uncompressObjects
  , uncompressDocument
  ) where

import Data.Context (ctx)
import Data.Fallible (FallibleT)
import Data.Functor ((<&>))
import Data.Logging (Logging, sayF)

import Pdf.Document.Document (PDFDocument, fromList, toList)
import Pdf.Document.ObjectStream (explodeDocument, explodeObjects)
import Pdf.Document.PDFObjects (PDFObjects)
import Pdf.Object.Unfilter (unfilter)

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
  let context = ctx ("uncompressobjects" :: String)

  sayF context "Extracting objects from object streams"
  objects <- explodeObjects pdf

  sayF context "Unfiltering all objects"
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
  let context = ctx ("uncompressdocument" :: String)
  sayF context "Extracting objects from object streams"
  objects <- explodeDocument pdf <&> toList

  sayF context "Unfiltering all objects"
  fromList <$> mapM unfilter objects
