module Pdf.Object.FilterCombine.RleLzw
  ( rleLzw
  ) where

import Codec.Compression.LZW qualified as LZW

import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.UnifiedError (UnifiedError)

import Pdf.Object.Container (Filter (Filter))
import Pdf.Object.FilterCombine.FilterCombination
    ( FilterCombination
    , fcBytes
    , mkFCAppend
    )
import Pdf.Object.FilterCombine.Rle (rle)
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))

rleLzw
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError FilterCombination
rleLzw _ stream = do
  rle Nothing stream
    >>= LZW.compress . fcBytes
    <&> mkFCAppend
      [ Filter (PDFName "LZWDecode")       PDFNull
      , Filter (PDFName "RunLengthDecode") PDFNull
      ]
