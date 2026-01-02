{-|
Extracts and writes the raw stream of a PDF indirect object by number.

This module provides 'extract', which searches a 'PDFDocument' for the
specified object number. If the object contains a stream
('PDFIndirectObjectWithStream' or 'PDFObjectStream'), the stream is
"unfiltered" (decoded) and written to standard output. If the object is found
but does not have a stream, 'ObjectStreamNotFound' is raised; if it is not
found at all, 'ObjectNotFound' is raised. Errors are managed within the
'FallibleT IO' and 'PDFWork' contexts.
-}
module Command.Extract
  ( extract
  ) where

import Control.Monad.IO.Class (liftIO)

import Data.ByteString qualified as BS
import Data.Fallible (FallibleT)
import Data.List (find)
import Data.PDF.PDFDocument (PDFDocument)
import Data.PDF.PDFObject
    ( PDFObject (PDFIndirectObject, PDFIndirectObjectWithStream, PDFObjectStream)
    )
import Data.PDF.PDFWork (evalPDFWork, throwError)
import Data.UnifiedError (UnifiedError (ObjectNotFound, ObjectStreamNotFound))

import PDF.Object.State (getStream)
import PDF.Processing.Unfilter (unfilter)

{-|
Predicate that matches a 'PDFObject' by its indirect object number.

Returns 'True' if the object number equals the provided 'Int'. It handles
objects with and without associated streams by inspecting the relevant
constructors.
-}
objectWithNumber :: Int -> PDFObject -> Bool
objectWithNumber n (PDFIndirectObjectWithStream num _ _ _) = n == num
objectWithNumber n (PDFObjectStream num _ _ _)             = n == num
objectWithNumber n (PDFIndirectObject num _ _)             = n == num
objectWithNumber _ _                                       = False

{-|
Extract the decoded stream of the object with the given number and write it to
standard output.

Behavior:

* Searches the 'PDFDocument' for the matching indirect object number.
* If the object has a stream, applies 'unfilter' to decode it, then 'getStream'
  to obtain the bytes, and writes them to stdout via 'liftIO'/'BS.putStr'.
* If the object exists but has no stream, raises 'ObjectStreamNotFound'.
* If no object with the given number exists, raises 'ObjectNotFound'.

Side effects: writes to stdout within 'FallibleT IO' evaluated by 'evalPDFWork'.
-}
extract :: Int -> PDFDocument -> FallibleT IO ()
extract objectNumber objects = evalPDFWork $ do
  case find (objectWithNumber objectNumber) objects of
    Just object@PDFIndirectObjectWithStream{} ->
      unfilter object >>= getStream >>= liftIO . BS.putStr
    Just object@PDFObjectStream{} ->
      unfilter object >>= getStream >>= liftIO . BS.putStr
    Just _  -> throwError ObjectStreamNotFound
    Nothing -> throwError ObjectNotFound
