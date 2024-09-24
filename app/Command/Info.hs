module Command.Info
  ( showInfo
  ) where

import Data.Text.Lazy qualified as T

import Formatting (format, int, (%))

import Pdf.Document.Document (PDFDocument)
import Pdf.Object.Object (objectInfo)

import Util.Context (ctx)
import Util.Logging (sayF)
import Util.UnifiedError (FallibleT)

showInfo :: PDFDocument -> FallibleT IO ()
showInfo document = do
  let context = ctx ("showinfo" :: String)
  sayF context . T.toStrict $ format ("Found " % int % " objects") (length document)
  mapM_ (sayF context . T.toStrict . objectInfo) document
