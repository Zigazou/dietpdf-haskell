module Pdf.Object.FilterCombine.Jpeg2k
  ( jpeg2k
  ) where

import Control.Monad.Trans.Except (throwE)

import Data.ByteString qualified as BS

import External.PamToJpeg2k (pamToJpeg2k)

import Pdf.Object.Container (Filter (Filter), FilterList)
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))

import Util.Array (mkArray)
import Util.Logging (Logging)
import Util.UnifiedError (FallibleT, UnifiedError (UnsupportedFeature))

jpeg2k
  :: Logging IO
  => Maybe (Int, Int, Int, BS.ByteString)
  -> BS.ByteString
  -> FallibleT IO (FilterList, BS.ByteString)
jpeg2k (Just (width, height, depth, tupltype)) stream = do
  compressed <- pamToJpeg2k width height depth tupltype stream
  return (mkArray [Filter (PDFName "JPXDecode") PDFNull], compressed)
jpeg2k Nothing _ =
  throwE (UnsupportedFeature "No width and height information for JPEG2000")
