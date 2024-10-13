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

import Pdf.Object.State (getStream)
import Pdf.Processing.Unfilter (unfilter)

objectWithNumber :: Int -> PDFObject -> Bool
objectWithNumber n (PDFIndirectObjectWithStream num _ _ _) = n == num
objectWithNumber n (PDFObjectStream num _ _ _)             = n == num
objectWithNumber n (PDFIndirectObject num _ _)             = n == num
objectWithNumber _ _                                       = False

extract :: Int -> PDFDocument -> FallibleT IO ()
extract objectNumber objects = evalPDFWork $ do
  case find (objectWithNumber objectNumber) objects of
    Just object@PDFIndirectObjectWithStream{} ->
      unfilter object >>= getStream >>= liftIO . BS.putStr
    Just object@PDFObjectStream{} ->
      unfilter object >>= getStream >>= liftIO . BS.putStr
    Just _  -> throwError ObjectStreamNotFound
    Nothing -> throwError ObjectNotFound
