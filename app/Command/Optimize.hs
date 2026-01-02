{-|
Optimizes a PDF document and writes the result to a target file.

This module exposes 'optimize', which logs a summary of the number of objects
found, prepares interpreter work data with user 'Settings', and encodes the
document via 'pdfEncode'. The resulting bytes are written to the specified
output path.
-}
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

{-|
Optimize and encode a 'PDFDocument' using the provided 'Settings', writing the
result to the given output file path.

Behavior:

* Logs a message indicating the number of objects in the input document.
* Initializes the interpreter work data by applying 'setSettings' to
  'emptyWorkData'.
* Runs 'pdfEncode' within a state transformer to produce the optimized bytes.
* Writes the encoded PDF bytes to the specified output path.

Side effects: logs via 'sayF' and writes to the filesystem.
-}
optimize :: FilePath -> PDFDocument -> Settings -> FallibleT IO ()
optimize outputPDF objects settings = do
  sayF (Context "optimize")
       (L.toStrict $ format ("Found " % int % " objects") (length objects))
  evalStateT (pdfEncode objects) (setSettings emptyWorkData settings)
    >>= lift . BS.writeFile outputPDF
