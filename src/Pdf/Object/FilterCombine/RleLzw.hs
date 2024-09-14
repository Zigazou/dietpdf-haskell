module Pdf.Object.FilterCombine.RleLzw
  ( rleLzw
  ) where

import Codec.Compression.LZW qualified as LZW

import Data.ByteString qualified as BS

import Pdf.Object.Container (Filter (Filter), FilterList)
import Pdf.Object.FilterCombine.Rle (rle)
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))

import Util.Array (mkArray)
import Util.UnifiedError (UnifiedError)

rleLzw
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError (FilterList, BS.ByteString)
rleLzw _ stream = do
  compressed <- rle Nothing stream >>= LZW.compress . snd
  return
    ( mkArray
      [ Filter (PDFName "LZWDecode")       PDFNull
      , Filter (PDFName "RunLengthDecode") PDFNull
      ]
    , compressed
    )
