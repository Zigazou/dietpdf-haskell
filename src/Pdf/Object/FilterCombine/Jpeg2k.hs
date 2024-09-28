module Pdf.Object.FilterCombine.Jpeg2k
  ( jpeg2k
  ) where

import Control.Monad.Trans.Except (throwE)

import Data.ByteString qualified as BS
import Data.ColorSpace (ColorSpace)
import Data.Fallible (FallibleT)
import Data.Functor ((<&>))
import Data.Logging (Logging)
import Data.UnifiedError (UnifiedError (UnsupportedFeature))

import External.TiffToJpeg2k (tiffToJpeg2k)

import Pdf.Object.Container (Filter (Filter))
import Pdf.Object.FilterCombine.FilterCombination
    ( FilterCombination
    , mkFCAppend
    )
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))

jpeg2k
  :: Logging IO
  => Maybe (Int, Int, ColorSpace)
  -> BS.ByteString
  -> FallibleT IO FilterCombination
jpeg2k (Just (width, height, colorSpace)) stream =
  tiffToJpeg2k width height colorSpace stream
    <&> mkFCAppend [Filter (PDFName "JPXDecode") PDFNull]
jpeg2k Nothing _ =
  throwE (UnsupportedFeature "No width and height information for JPEG2000")
