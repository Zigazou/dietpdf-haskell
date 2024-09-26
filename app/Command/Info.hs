module Command.Info
  ( showInfo
  ) where

import Data.Context (ctx)
import Data.Fallible (FallibleT)
import Data.Logging (sayF)
import Data.Text.Lazy qualified as T

import Formatting (format, int, (%))

import Pdf.Document.Document (PDFDocument)
import Pdf.Object.Object (objectInfo)

showInfo :: PDFDocument -> FallibleT IO ()
showInfo document = do
  let context = ctx ("showinfo" :: String)
  sayF context . T.toStrict $ format ("Found " % int % " objects") (length document)
  mapM_ (sayF context . T.toStrict . objectInfo) document
