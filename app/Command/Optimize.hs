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
import Data.Text.Lazy qualified as L

import Formatting (format, int, (%))

import PDF.Document.Encode (pdfEncode)
import Data.Logging (sayF)
import Data.Context (Context(Context))

optimize :: FilePath -> PDFDocument -> Settings -> FallibleT IO ()
optimize outputPDF objects settings = do
  sayF (Context "optimize")
       (L.toStrict $ format ("Found " % int % " objects") (length objects))
  evalStateT (pdfEncode objects) (setSettings emptyWorkData settings)
    >>= lift . BS.writeFile outputPDF
