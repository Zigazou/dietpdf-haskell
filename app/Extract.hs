{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
                                                )
import           Pdf.Object.Unfilter            ( unfilter )
import           Util.UnifiedError              ( FallibleT
                                                , UnifiedError
                                                  ( ObjectNotFound
                                                  , ObjectStreamNotFound
                                                  )
                                                )
import           Control.Monad.Trans.Except     ( throwE )
import           Control.Monad.Trans.Class      ( lift )

extract :: Int -> PDFDocument -> FallibleT IO ()
extract objectNumber objects = do
  case find (objectWithNumber objectNumber) objects of
    Just object@PDFIndirectObjectWithStream{} -> unfilter object >>= \case
      (PDFIndirectObjectWithStream _ _ _ unfilteredStream) ->
        lift $ BS.putStr unfilteredStream
      _anyotherValue -> lift $ BS.putStr ""
    Just object@PDFObjectStream{} -> unfilter object >>= \case
      (PDFObjectStream _ _ _ unfilteredStream) ->
        lift $ BS.putStr unfilteredStream
      _anyotherValue -> lift $ BS.putStr ""
    Just _  -> throwE ObjectStreamNotFound
    Nothing -> throwE ObjectNotFound

 where
  objectWithNumber :: Int -> PDFObject -> Bool
  objectWithNumber n (PDFIndirectObjectWithStream num _ _ _) = n == num
  objectWithNumber n (PDFObjectStream num _ _ _) = n == num
  objectWithNumber n (PDFIndirectObject num _ _) = n == num
  objectWithNumber _ _ = False
