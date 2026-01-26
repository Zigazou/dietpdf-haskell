{-|
Filter candidates for JPEG (@DCTDecode@) content streams.

For JPEG binary data, we generally avoid re-compressing with Deflate unless
it helps, and we may re-encode to JPEG 2000 (@JPXDecode@) for better
compression.

Evaluated candidates:

* No-op
* Deflate/Zopfli
* Re-encode to JPEG 2000 at moderate quality

When image metadata is available, JPEG 2000 conversion is attempted and all
results are logged for comparison.
-}
module PDF.Processing.ApplyFilter.JPG
  (applyEveryFilterJPG) where


import Control.Monad.State (gets, lift)
import Control.Monad.Trans.Except (except)

import Data.Bitmap.BitmapConfiguration (BitmapConfiguration)
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Logging (Logging)
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination
  (FilterCombination, fcBytes, mkFCAppend, mkFCReplace)
import Data.PDF.PDFObject (PDFObject (PDFName, PDFNull))
import Data.PDF.PDFWork (PDFWork)
import Data.PDF.Settings (sCompressor)
import Data.PDF.WorkData (wSettings)

import External.JpegToJpeg2k (jpegToJpeg2k)

import PDF.Processing.ApplyFilter.Helpers (filterInfo, filterInfoCompressor)
import PDF.Processing.FilterCombine.Compressor (compressor)

{-|
Evaluate filter candidates specifically for JPEG content streams.

When width/components are available, tries Zopfli/Deflate and a re-encoding to
JPEG2000 (`JPXDecode`) with moderate quality; otherwise, falls back to
Zopfli/Deflate.
-}
applyEveryFilterJPG
  :: Logging IO
  => Bool
  -> Maybe BitmapConfiguration
  -> ByteString
  -> PDFWork IO [FilterCombination]
applyEveryFilterJPG _objectIsAMask (Just _imageProperty) stream = do
  useCompressor <- gets (sCompressor . wSettings)
  let rNothing = mkFCAppend [] stream

  rCompressor <- lift (except $ compressor Nothing stream useCompressor)
  filterInfoCompressor useCompressor "" stream (fcBytes rCompressor)

  -- Try Jpeg2000 for images with less than 4 components.
  let jpeg2kQuality :: Int
      jpeg2kQuality = 60

  rJpeg2k <- lift (jpegToJpeg2k jpeg2kQuality stream)
         <&> mkFCReplace [Filter (PDFName "JPXDecode") PDFNull]
  filterInfo "JPEG2000" stream (fcBytes rJpeg2k)

  return [rNothing, rCompressor, rJpeg2k]

applyEveryFilterJPG _objectIsAMask Nothing stream = do
  useCompressor <- gets (sCompressor . wSettings)
  let rNothing = mkFCAppend [] stream

  rCompressor <- lift (except $ compressor Nothing stream useCompressor)
  filterInfoCompressor useCompressor "" stream (fcBytes rCompressor)

  return [rNothing, rCompressor]