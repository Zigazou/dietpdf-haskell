-- | Clean a PDF file using PDFTK.
module Util.PDFTKClean (pdftkClean) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

import Data.Text qualified as T

import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)

import Util.Logging (sayF)
import Util.UnifiedError (FallibleT, UnifiedError (PDFTKError))

pdftkClean :: FilePath -> FilePath -> FallibleT IO ()
pdftkClean inputPdf outputPdf = do
  sayF $ T.concat ["Cleaning with PDFTK for ", T.pack inputPdf]

  (rc, _, _) <- lift $ readProcessWithExitCode "pdftk"
                  [ inputPdf
                  , "cat"
                  , "output"
                  , outputPdf
                  ] ""

  case rc of
    ExitSuccess -> return ()
    _           -> throwE (PDFTKError "PDFTK failed")
