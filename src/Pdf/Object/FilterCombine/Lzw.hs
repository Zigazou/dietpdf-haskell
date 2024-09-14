module Pdf.Object.FilterCombine.Lzw
  ( lzw
  ) where

import Codec.Compression.LZW qualified as LZW

import Data.ByteString qualified as BS

import Pdf.Object.Container (Filter (Filter), FilterList)
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))

import Util.Array (mkArray)
import Util.UnifiedError (UnifiedError)

lzw
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError (FilterList, BS.ByteString)
lzw _ stream = do
  compressed <- LZW.compress stream
  return (mkArray [Filter (PDFName "LZWDecode") PDFNull], compressed)
