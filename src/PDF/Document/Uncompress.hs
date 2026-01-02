{-|
Uncompress and extract PDF objects from compressed streams.

Provides utilities for decompressing PDF documents by extracting objects
embedded in object streams and removing stream filters from all objects.
-}
module PDF.Document.Uncompress
  ( uncompressObjects
  , uncompressDocument
  ) where

import Data.Context (ctx)
import Data.Functor ((<&>))
import Data.Logging (Logging)
import Data.PDF.PDFDocument (PDFDocument, fromList, toList)
import Data.PDF.PDFObjects (PDFObjects)
import Data.PDF.PDFWork (PDFWork, sayP, withContext)

import PDF.Document.ObjectStream (explodeDocument, explodeObjects)
import PDF.Processing.Unfilter (unfilter)

{-|
Uncompress all objects in a PDF object collection.

Extracts objects embedded in object streams and removes those streams. Applies
stream filters (unfilter) to all objects. Objects that fail decompression are
left in their original state. Thus the function may leave some objects
compressed if errors occur during processing.
-}
uncompressObjects :: Logging m => PDFObjects -> PDFWork m PDFObjects
uncompressObjects pdf = withContext (ctx ("uncompressobjects" :: String)) $ do
  sayP "Extracting objects from object streams"
  objects <- explodeObjects pdf

  sayP "Unfiltering all objects"
  mapM unfilter objects

{-|
Uncompress all objects in a PDF document.

Extracts objects embedded in object streams and removes those streams. Applies
stream filters (unfilter) to all objects. Objects that fail decompression are
left in their original state. Thus the function may leave some objects
compressed if errors occur during processing. Returns a new document with all
successfully decompressed objects.
-}
uncompressDocument :: Logging m => PDFDocument -> PDFWork m PDFDocument
uncompressDocument pdf = withContext (ctx ("uncompressDocument" :: String)) $ do
  sayP "Extracting objects from object streams"
  objects <- explodeDocument pdf <&> toList

  sayP "Unfiltering all objects"
  fromList <$> mapM unfilter objects
