-- | Optimize a PDF file using GhostScript.
module External.GSOptimize (gsOptimize) where

import Data.Text qualified as T

import External.ExternalCommand (externalCommand)

import Util.Logging (sayF)
import Util.UnifiedError (FallibleT)

{-
https://askubuntu.com/questions/113544/how-can-i-reduce-the-file-size-of-a-scanned-pdf-file
#!/bin/sh
INPUT=$1; shift
OUTPUT=$1; shift
GS_BIN=/usr/bin/gs
QFACTOR="0.40"

# Image Compression Quality
#
# Quality HSamples VSamples QFactor
# Minimum [2 1 1 2] [2 1 1 2] 2.40
# Low     [2 1 1 2] [2 1 1 2] 1.30
# Medium  [2 1 1 2] [2 1 1 2] 0.76
# High    [1 1 1 1] [1 1 1 1] 0.40
# Maximum [1 1 1 1] [1 1 1 1] 0.15

${GS_BIN} \
  -dBATCH \
  -dSAFER \
  -DNOPAUSE \
  -q \
  -sDEVICE=pdfwrite \
  -sOutputFile=${OUTPUT} \
  -c "<< /ColorImageDict << /QFactor ${QFACTOR} /Blend 1 /HSample [1 1 1 1] /VSample [1 1 1 1] >> >> setdistillerparams" \
  -f ${INPUT}
-}


gsOptions :: [(String, String, String)]
gsOptions =
  [ ("s", "DEVICE", "pdfwrite")                   -- output device
  , ("d", "DetectDuplicateImages", "true")        -- group identical images
  , ("d", "AutoRotatePages", "/None")             -- don't rotate pages
  , ("d", "CompressFonts", "true")                -- compress fonts
  , ("d", "DownsampleColorImages", "true")        -- downsample color images
  , ("d", "DownsampleGrayImages", "true")         -- downsample gray images
  , ("d", "DownsampleMonoImages", "true")         -- downsample monochrome images
  , ("d", "ColorImageResolution", "200")          -- color image resolution
  , ("d", "GrayImageResolution", "200")           -- gray image resolution
  , ("d", "MonoImageResolution", "200")           -- monochrome image resolution
  , ("d", "ColorImageDownsampleThreshold", "1.0") -- color image downsample threshold
  , ("d", "GrayImageDownsampleThreshold", "1.0")  -- gray image downsample threshold
  , ("d", "MonoImageDownsampleThreshold", "1.0")  -- b&w downsample threshold
  , ("d", "ColorImageDownsampleType", "/Bicubic") -- color image downsample type
  , ("d", "GrayImageDownsampleType", "/Bicubic")  -- gray image downsample type
  , ("d", "MonoImageDownsampleType", "/Bicubic")  -- b&w image downsample type
  , ("d", "ColorImageFilter", "/FlateEncode")     -- color image filter
  , ("d", "CompatibilityLevel", "1.7")            -- PDF compatibility level
  , ("d", "ConvertCMYKImagesToRGB", "true")       -- convert CMYK images to RGB
  , ("d", "MaxInlineImageSize", "0")              -- disable inline images
  , ("d", "QUIET", "")                            -- quiet mode
  , ("d", "SAFER", "")                            -- safe mode
  , ("d", "NOPAUSE", "")                          -- no pause
  , ("d", "BATCH", "")                            -- batch mode
  ]

generateOptions :: [(String, String, String)] -> [String]
generateOptions = fmap generateOption
 where
  generateOption :: (String, String, String) -> String
  generateOption (switch, option, "")    = concat ["-", switch, option]
  generateOption (switch, option, value) = concat ["-", switch, option, "=", value]

gsOptimize :: FilePath -> FilePath -> FallibleT IO ()
gsOptimize inputPdf outputPdf = do
  sayF $ T.concat ["Running GhostScript on ", T.pack inputPdf]

  externalCommand "gs"
    ( generateOptions gsOptions ++ ["-sOutputFile=" ++ outputPdf, inputPdf] )
