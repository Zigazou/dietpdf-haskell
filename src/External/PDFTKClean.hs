-- | Clean a PDF file using PDFTK.
module External.PDFTKClean (pdftkClean) where

import Data.Text qualified as T

import External.ExternalCommand (externalCommand)

import Util.Logging (sayF)
import Util.UnifiedError (FallibleT)

pdftkClean :: FilePath -> FilePath -> FallibleT IO ()
pdftkClean inputPdf outputPdf = do
  sayF $ T.concat ["Cleaning with PDFTK for ", T.pack inputPdf]

  externalCommand "pdftk" [ inputPdf, "cat", "output", outputPdf, "compress" ]