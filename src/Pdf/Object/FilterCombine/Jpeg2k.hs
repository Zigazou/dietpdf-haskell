module Pdf.Object.FilterCombine.Jpeg2k
  ( jpeg2k
  ) where

import Control.Monad.Trans.Except (throwE)

import Data.ByteString qualified as BS
import Data.Functor ((<&>))

import External.PamToJpeg2k (pamToJpeg2k)

import Pdf.Object.Container (Filter (Filter))
import Pdf.Object.FilterCombine.FilterCombination
    ( FilterCombination
    , mkFCAppend
    )
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))

import Util.Logging (Logging)
import Util.UnifiedError (FallibleT, UnifiedError (UnsupportedFeature))

jpeg2k
  :: Logging IO
  => Maybe (Int, Int, Int, BS.ByteString)
  -> BS.ByteString
  -> FallibleT IO FilterCombination
jpeg2k (Just (width, height, depth, tupltype)) stream =
  pamToJpeg2k width height depth tupltype stream
    <&> mkFCAppend [Filter (PDFName "JPXDecode") PDFNull]
jpeg2k Nothing _ =
  throwE (UnsupportedFeature "No width and height information for JPEG2000")
