-- | Optimize a PDF file using PDFToCairo.
module External.PDFToCairoOptimize (pdfToCairoOptimize) where

import Data.Context (Contextual (ctx))
import Data.Logging (sayF)
import Data.Text qualified as T
import Data.Fallible (FallibleT)

import External.ExternalCommand (externalCommand)

{- |
Optimize a PDF file using PDFToCairo.

This function is a wrapper around the `pdftocairo` command-line tool.
-}
pdfToCairoOptimize :: FilePath -> FilePath -> FallibleT IO ()
pdfToCairoOptimize inputPdf outputPdf = do
  sayF (ctx ("pdftocairo" :: String))
     $ T.concat ["Running pdftocairo on ", T.pack inputPdf]

  externalCommand "pdftocairo" [inputPdf, "-pdf", outputPdf]
