{-|
JPEG 2000 re-encoding filter combination.

Wraps `tiffToJpeg2k` to convert raw image-like streams with known width and
height into `JPXDecode` encoded bytes, returning a `FilterCombination`. Useful
for reducing size of bitmap images.
-}
module PDF.Processing.FilterCombine.Jpeg2k
  ( jpeg2k
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.State (lift)

import Data.ByteString (ByteString)
import Data.ColorSpace (ColorSpace)
import Data.Functor ((<&>))
import Data.Logging (Logging)
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName, PDFNull))
import Data.PDF.PDFWork (PDFWork)
import Data.UnifiedError (UnifiedError (UnsupportedFeature))

import External.TiffToJpeg2k (tiffToJpeg2k)

{-|
Re-encode a stream to JPEG 2000 (`JPXDecode`).

Parameters:

* Optional quality factor.
* Optional triple of width, height, and `ColorSpace`.
* Source bytes.

Fails with `UnsupportedFeature` when width/height is missing.
-}
jpeg2k
  :: Logging IO
  => Maybe Int
  -> Maybe (Int, Int, ColorSpace)
  -> ByteString
  -> PDFWork IO FilterCombination
jpeg2k quality (Just (width, height, colorSpace)) stream =
  lift (tiffToJpeg2k quality width height colorSpace stream)
    <&> mkFCAppend [Filter (PDFName "JPXDecode") PDFNull]
jpeg2k _quality Nothing _stream =
  throwError (UnsupportedFeature "No width and height information for JPEG2000")
