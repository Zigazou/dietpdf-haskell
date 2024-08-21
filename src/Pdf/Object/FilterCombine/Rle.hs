module Pdf.Object.FilterCombine.Rle
  ( rle
  ) where

import Codec.Compression.RunLength qualified as RL

import Data.ByteString qualified as BS

import Pdf.Object.Container (Filter (Filter), FilterList)
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))

import Util.Array (mkArray)
import Util.UnifiedError (UnifiedError)

rle
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError (FilterList, BS.ByteString)
rle _ stream = do
  compressed <- RL.compress stream
  return (mkArray [Filter (PDFName "RunLengthDecode") PDFNull], compressed)
