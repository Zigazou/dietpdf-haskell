-- | Optimize a PDF file using GhostScript.
module Util.GSOptimize (gsOptimize) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

import Data.Text qualified as T

import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)

import Util.Logging (sayF)
import Util.UnifiedError (FallibleT, UnifiedError (GhostScriptError))
gsOptions :: [(String, String)]
gsOptions =
  [ ("DetectDuplicateImages", "true")        -- group identical images
  , ("AutoRotatePages", "/None")             -- don't rotate pages
  , ("CompressFonts", "true")                -- compress fonts
  , ("DownsampleColorImages", "true")        -- downsample color images
  , ("DownsampleGrayImages", "true")         -- downsample gray images
  , ("DownsampleMonoImages", "true")         -- downsample monochrome images
  , ("ColorImageResolution", "300")          -- color image resolution
  , ("GrayImageResolution", "300")           -- gray image resolution
  , ("MonoImageResolution", "300")           -- monochrome image resolution
  , ("ColorImageDownsampleThreshold", "1.0") -- color image downsample threshold
  , ("GrayImageDownsampleThreshold", "1.0")  -- gray image downsample threshold
  , ("MonoImageDownsampleThreshold", "1.0")  -- b&w downsample threshold
  , ("ColorImageDownsampleType", "/Bicubic") -- color image downsample type
  , ("GrayImageDownsampleType", "/Bicubic")  -- gray image downsample type
  , ("MonoImageDownsampleType", "/Bicubic")  -- b&w image downsample type
  , ("ColorImageFilter", "/FlateEncode")     -- color image filter
  , ("CompatibilityLevel", "1.7")            -- PDF compatibility level
  ]

generateOptions :: [(String, String)] -> [String]
generateOptions = fmap (\(option, value) -> concat ["-d", option, "=", value])

gsOptimize :: FilePath -> FilePath -> FallibleT IO ()
gsOptimize inputPdf outputPdf = do
  sayF $ T.concat ["Running GhostScript on ", T.pack inputPdf]

  (rc, _, _) <- lift $ readProcessWithExitCode "gs"
                  ( ["-sDEVICE=pdfwrite"]
                  ++ generateOptions gsOptions
                  ++ [ "-dQUIET"
                     , "-dNOPAUSE"
                     , "-dBATCH"
                     , "-o", outputPdf
                     , inputPdf
                     ]
                  ) ""

  case rc of
    ExitSuccess -> return ()
    _           -> throwE (GhostScriptError "GhostScript failed")
