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

jpeg2k
  :: Logging IO
  => Maybe (Int, Int, ColorSpace)
  -> ByteString
  -> PDFWork IO FilterCombination
jpeg2k (Just (width, height, colorSpace)) stream =
  lift (tiffToJpeg2k width height colorSpace stream)
    <&> mkFCAppend [Filter (PDFName "JPXDecode") PDFNull]
jpeg2k Nothing _ =
  throwError (UnsupportedFeature "No width and height information for JPEG2000")
