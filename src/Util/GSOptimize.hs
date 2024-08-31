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
                  , "-dDetectDuplicateImages=true" -- group identical images
                  , "-dAutoRotatePages=/None"      -- don't rotate pages
                  , "-dCompressFonts=true"         -- compress fonts
                  , "-dDownsampleColorImages=true" -- downsample color images
                  , "-dDownsampleGrayImages=true"  -- downsample gray images
                  , "-dDownsampleMonoImages=true"  -- downsample monochrome images
                  , "-dColorImageResolution=300"   -- color image resolution
                  , "-dGrayImageResolution=300"    -- gray image resolution
                  , "-dMonoImageResolution=300"    -- monochrome image resolution
                  , "-dColorImageDownsampleThreshold=1.0" -- color image downsample threshold
                  , "-dGrayImageDownsampleThreshold=1.0"  -- gray image downsample threshold
                  , "-dMonoImageDownsampleThreshold=1.0"  -- monochrome image downsample threshold
                  , "-dColorImageDownsampleType=/Bicubic" -- color image downsample type
                  , "-dGrayImageDownsampleType=/Bicubic"  -- gray image downsample type
                  , "-dMonoImageDownsampleType=/Bicubic"  -- monochrome image downsample type
                  , "-dColorImageFilter=/FlateEncode" -- color image filter
                  , "-dCompatibilityLevel=1.7"
                  , "-dQUIET"
                  , "-dNOPAUSE"
                  , "-dBATCH"
                  , "-o", outputPdf
                  , inputPdf
                  ] ""

  case rc of
    ExitSuccess -> return ()
    _           -> throwE (GhostScriptError "GhostScript failed")
