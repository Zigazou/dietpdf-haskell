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

{- |
Uncompress all `PDFObject` contained in a `PDFDObjects`.

Objects embedded in object streams are extracted and the object stream is
removed.

If a `PDFObject` cannot be uncompressed (meaning its processing generated an
error), the object is left as is. Thus this function may leave object
uncompressed.
-}
uncompressObjects :: Logging m => PDFObjects -> PDFWork m PDFObjects
uncompressObjects pdf = withContext (ctx ("uncompressobjects" :: String)) $ do
  sayP "Extracting objects from object streams"
  objects <- explodeObjects pdf

  sayP "Unfiltering all objects"
  mapM unfilter objects

{- |
Uncompress all `PDFObject` contained in a `PDFDocument`.

Objects embedded in object streams are extracted and the object stream is
removed.

If a `PDFObject` cannot be uncompressed (meaning its processing generated an
error), the object is left as is. Thus this function may leave object
uncompressed.
-}
uncompressDocument :: Logging m => PDFDocument -> PDFWork m PDFDocument
uncompressDocument pdf = withContext (ctx ("uncompressDocument" :: String)) $ do
  sayP "Extracting objects from object streams"
  objects <- explodeDocument pdf <&> toList

  sayP "Unfiltering all objects"
  fromList <$> mapM unfilter objects
