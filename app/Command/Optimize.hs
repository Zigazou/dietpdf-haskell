module Command.Optimize
  ( optimize
  ) where

import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Class (MonadTrans (lift))

import Data.ByteString qualified as BS
import Data.Fallible (FallibleT)
import Data.PDF.PDFDocument (PDFDocument)
import Data.PDF.Settings (Settings)
import Data.PDF.WorkData (emptyWorkData, setSettings)
import Data.Text.Lazy.IO qualified as T

import Formatting (format, int, (%))

import PDF.Document.Encode (pdfEncode)

optimize :: FilePath -> PDFDocument -> Settings -> FallibleT IO ()
optimize outputPDF objects settings = do
  lift . T.putStrLn $ format ("Found " % int % " objects") (length objects)
  evalStateT (pdfEncode objects) (setSettings emptyWorkData settings)
    >>= lift . BS.writeFile outputPDF
