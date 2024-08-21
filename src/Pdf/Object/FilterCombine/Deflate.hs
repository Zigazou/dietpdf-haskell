module Pdf.Object.FilterCombine.Deflate
  ( deflate
  ) where

import Codec.Compression.Flate qualified as FL

import Data.ByteString qualified as BS

import Pdf.Object.Container (Filter (Filter), FilterList)
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))

import Util.Array (mkArray)
import Util.UnifiedError (UnifiedError)

deflate
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError (FilterList, BS.ByteString)
deflate _ stream = do
  compressed <- FL.fastCompress stream
  return (mkArray [Filter (PDFName "FlateDecode") PDFNull], compressed)
