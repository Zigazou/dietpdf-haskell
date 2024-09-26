-- | Clean a PDF file using PDFTK.
module External.PDFTKClean (pdftkClean) where

import Data.Context (Contextual (ctx))
import Data.Fallible (FallibleT)
import Data.Logging (sayF)
import Data.Text qualified as T

import External.ExternalCommand (externalCommand)

pdftkClean :: FilePath -> FilePath -> FallibleT IO ()
pdftkClean inputPdf outputPdf = do
  sayF (ctx ("pdftk" :: String))
     $ T.concat ["Cleaning with PDFTK for ", T.pack inputPdf]

  externalCommand "pdftk" [ inputPdf, "cat", "output", outputPdf, "compress" ]
