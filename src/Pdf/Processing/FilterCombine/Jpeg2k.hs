module Pdf.Processing.FilterCombine.Jpeg2k
  ( jpeg2k
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.State (lift)

import Data.ByteString qualified as BS
import Data.ColorSpace (ColorSpace)
import Data.Functor ((<&>))
import Data.Logging (Logging)
import Data.PDF.PDFWork (PDFWork)
import Data.UnifiedError (UnifiedError (UnsupportedFeature))

import External.TiffToJpeg2k (tiffToJpeg2k)

import Pdf.Object.Container (Filter (Filter))
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))
import Pdf.Processing.FilterCombine.FilterCombination
    ( FilterCombination
    , mkFCAppend
    )

jpeg2k
  :: Logging IO
  => Maybe (Int, Int, ColorSpace)
  -> BS.ByteString
  -> PDFWork IO FilterCombination
jpeg2k (Just (width, height, colorSpace)) stream =
  lift (tiffToJpeg2k width height colorSpace stream)
    <&> mkFCAppend [Filter (PDFName "JPXDecode") PDFNull]
jpeg2k Nothing _ =
  throwError (UnsupportedFeature "No width and height information for JPEG2000")
