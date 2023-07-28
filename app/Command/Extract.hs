module Command.Extract
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
import           Pdf.Object.State               ( getStream )
import           Pdf.Object.Unfilter            ( unfilter )
import           Util.UnifiedError              ( FallibleT
                                                , UnifiedError
                                                  ( ObjectNotFound
                                                  , ObjectStreamNotFound
                                                  )
                                                )
import           Control.Monad.Trans.Except     ( throwE )
import           Control.Monad.Trans.Class      ( lift )

objectWithNumber :: Int -> PDFObject -> Bool
objectWithNumber n (PDFIndirectObjectWithStream num _ _ _) = n == num
objectWithNumber n (PDFObjectStream num _ _ _) = n == num
objectWithNumber n (PDFIndirectObject num _ _) = n == num
objectWithNumber _ _ = False

extract :: Int -> PDFDocument -> FallibleT IO ()
extract objectNumber objects =
  case find (objectWithNumber objectNumber) objects of
    Just object@PDFIndirectObjectWithStream{} ->
      unfilter object >>= getStream >>= lift . BS.putStr
    Just object@PDFObjectStream{} ->
      unfilter object >>= getStream >>= lift . BS.putStr
    Just _  -> throwE ObjectStreamNotFound
    Nothing -> throwE ObjectNotFound
