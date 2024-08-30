-- | Optimize a PDF file using GhostScript.
module Util.GSOptimize (gsOptimize) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

import Data.Text qualified as T

import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)

import Util.Logging (sayF)
import Util.UnifiedError (FallibleT, UnifiedError (GhostScriptError))

gsOptimize :: FilePath -> FilePath -> FallibleT IO ()
gsOptimize inputPdf outputPdf = do
  sayF $ T.concat ["Running GhostScript on ", T.pack inputPdf]

  (rc, _, _) <- lift $ readProcessWithExitCode "gs"
                  [ "-sDEVICE=pdfwrite"
                  , "-dDetectDuplicateImages=true"
                  , "-dQUIET"
                  , "-dNOPAUSE"
                  , "-dBATCH"
                  , "-o", outputPdf
                  , inputPdf
                  ] ""

  case rc of
    ExitSuccess -> return ()
    _           -> throwE (GhostScriptError "GhostScript failed")
