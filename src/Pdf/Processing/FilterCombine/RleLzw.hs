module Pdf.Processing.FilterCombine.RleLzw
  ( rleLzw
  ) where

import Codec.Compression.LZW qualified as LZW

import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Functor ((<&>))

import Pdf.Object.Container (Filter (Filter))
import Pdf.Processing.FilterCombine.Rle (rle)
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))
import Pdf.Processing.FilterCombine.FilterCombination
    ( FilterCombination
    , fcBytes
    , mkFCAppend
    )

rleLzw
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Fallible FilterCombination
rleLzw _ stream = do
  rle Nothing stream
    >>= LZW.compress . fcBytes
    <&> mkFCAppend
      [ Filter (PDFName "LZWDecode")       PDFNull
      , Filter (PDFName "RunLengthDecode") PDFNull
      ]
