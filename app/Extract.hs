{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Extract
  ( extract
  ) where

import qualified Data.ByteString               as BS
import           Data.List                      ( find )
import           Pdf.Document.Document          ( PDFDocument )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFObjectStream
                                                  )
                                                , updateE
                                                )
import           Pdf.Object.Unfilter            ( unfilter )
import           Util.Errors                    ( putErrorLn )

extract :: Int -> PDFDocument -> IO ()
extract objectNumber objects = do
  case find (objectWithNumber objectNumber) objects of
    Just object@PDFIndirectObjectWithStream{} ->
      case updateE object unfilter of
        Right (PDFIndirectObjectWithStream _ _ _ unfilteredStream) ->
          BS.putStr unfilteredStream
        _anyotherValue -> putErrorLn "Unable to unfilter stream"
    Just object@PDFObjectStream{} -> case updateE object unfilter of
      Right (PDFObjectStream _ _ _ unfilteredStream) ->
        BS.putStr unfilteredStream
      _anyotherValue -> putErrorLn "Unable to unfilter stream"
    Just _  -> putErrorLn "Object found but with no stream"
    Nothing -> putErrorLn "Object not found"
 where
  objectWithNumber :: Int -> PDFObject -> Bool
  objectWithNumber n (PDFIndirectObjectWithStream num _ _ _) = n == num
  objectWithNumber n (PDFObjectStream num _ _ _) = n == num
  objectWithNumber n (PDFIndirectObject num _ _) = n == num
  objectWithNumber _ _ = False
