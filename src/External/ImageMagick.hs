{-|
Optimize JPEG files using JpegTran.

Provides JPEG optimization via the `jpegtran` command-line tool, comparing and
selecting between progressive and baseline encoding modes.
-}
module External.ImageMagick (extractCbCrChannels, zeroFillJPEG) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Fallible (FallibleT)
import Data.UnifiedError (UnifiedError (ExternalCommandError))

import External.ExternalCommand (externalCommandBuf)

import GHC.IO.Handle (hClose)

import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO.Temp (withSystemTempFile)
import System.Process (readProcessWithExitCode)

{-|
Extract the Cb and Cr channels from a JPEG image in the YCbCr color space.

This is a wrapper around ImageMagick's `convert` command-line tool, used to
inspect pixel values (e.g. with `containsOnlyGray`) before deciding whether a
JPEG can be safely reduced to grayscale.
-}
extractCbCrChannels :: ByteString -> FallibleT IO (ByteString, ByteString)
extractCbCrChannels input = do
  output <- externalCommandBuf "convert"
                               ["jpg:-", "-colorspace", "YCbCr", "yuv:-"]
                               input

  let
    componentLength :: Int
    componentLength = BS.length output `div` 6

    cb_start :: Int
    cb_start = 4 * componentLength

    cr_start :: Int
    cr_start = 5 * componentLength

    cb_component :: ByteString
    cb_component = BS.take componentLength (BS.drop cb_start output)

    cr_component :: ByteString
    cr_component = BS.take componentLength (BS.drop cr_start output)

  return (cb_component, cr_component)

{-|
Wrap a raw 8-bit grayscale buffer into a headered PGM (P5) image, so that it
can be fed to ImageMagick as a mask without guessing dimensions.
-}
toGrayscalePGM :: Int -> Int -> ByteString -> ByteString
toGrayscalePGM width height raw = BS.concat
  [BSC.pack ("P5\n" ++ show width ++ " " ++ show height ++ "\n255\n"), raw]

{-|
Turn a raw soft-mask buffer into a strict black/white mask: any sample equal
to @0@ (fully transparent) stays @0@, every other sample becomes @255@.

This is what lets a plain ImageMagick @Multiply@ composite reproduce the
"zero-fill" semantics regardless of intermediate mask values, since only the
zero/non-zero distinction matters.
-}
binarizeMask :: ByteString -> ByteString
binarizeMask = BS.map (\sample -> if sample == 0 then 0 else 255)

{-|
Detect the JPEG quality (0-100) a JPEG image was encoded with, via
ImageMagick's `identify`. Defaults to @90@ when the value cannot be parsed.
-}
jpegQuality :: ByteString -> FallibleT IO Int
jpegQuality input = do
  output <- externalCommandBuf "identify" ["-format", "%Q", "jpg:-"] input
  case reads (BSC.unpack output) of
    [(quality, _anyOtherCase)] -> return quality
    _anyOtherCase              -> return 90

{-|
Zero-fill the pixels of a JPEG image that are hidden by a soft mask.

Given the raw (decoded) 8-bit grayscale soft mask matching the image's
@width@/@height@, every colour sample where the mask is exactly @0@ (fully
transparent) is replaced by zero, while every other pixel is left untouched.
The result is re-encoded as JPEG at the same quality as the input, detected
via ImageMagick `identify`.

This is a wrapper around ImageMagick's `convert`, run through temporary files
since `convert` needs to read two distinct input images (the JPEG and the
mask) to composite them.
-}
zeroFillJPEG :: Int -> Int -> ByteString -> ByteString -> FallibleT IO ByteString
zeroFillJPEG width height image mask = do
  quality <- jpegQuality image
  let maskPGM = toGrayscalePGM width height (binarizeMask mask)

  withSystemTempFile "dietpdf-zerofill-in.jpg" $ \imagePath imageHandle -> do
    withSystemTempFile "dietpdf-zerofill-mask.pgm" $ \maskPath maskHandle -> do
      withSystemTempFile "dietpdf-zerofill-out.jpg" $ \outputPath outputHandle -> do
        lift $ hClose imageHandle
        lift $ hClose maskHandle
        lift $ hClose outputHandle
        lift $ BS.writeFile imagePath image
        lift $ BS.writeFile maskPath maskPGM

        (exitCode, _stdout, _stderr) <- lift $ readProcessWithExitCode
          "convert"
          [ imagePath
          , maskPath
          , "-compose", "Multiply"
          , "-composite"
          , "-quality", show quality
          , outputPath
          ]
          ""

        case exitCode of
          ExitSuccess    -> lift $ BS.readFile outputPath
          ExitFailure rc -> throwE (ExternalCommandError "convert" rc)
