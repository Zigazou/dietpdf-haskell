module Command.Optimize
  ( optimize
  ) where

import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Class (MonadTrans (lift))

import Data.ByteString qualified as BS
import Data.Fallible (FallibleT)
import Data.PDF.PDFDocument (PDFDocument)
import Data.PDF.WorkData (emptyWorkData)
import Data.Text.Lazy.IO qualified as T

import Formatting (format, int, (%))

import Pdf.Document.Encode (pdfEncode)

optimize :: FilePath -> PDFDocument -> FallibleT IO ()
optimize outputPDF objects = do
  lift . T.putStrLn $ format ("Found " % int % " objects") (length objects)
  evalStateT (pdfEncode objects) emptyWorkData >>= lift . BS.writeFile outputPDF
