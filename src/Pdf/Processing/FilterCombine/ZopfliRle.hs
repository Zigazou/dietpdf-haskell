module Pdf.Processing.FilterCombine.ZopfliRle
  ( zopfliRle
  ) where

import Codec.Compression.Flate qualified as FL

import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Functor ((<&>))

import Pdf.Object.Container (Filter (Filter))
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))
import Pdf.Processing.FilterCombine.FilterCombination
    ( FilterCombination
    , fcBytes
    , mkFCAppend
    )
import Pdf.Processing.FilterCombine.Rle (rle)

zopfliRle
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Fallible FilterCombination
zopfliRle _ stream =
  FL.compress stream
    >>= rle Nothing
    <&> mkFCAppend
      [ Filter (PDFName "RunLengthDecode") PDFNull
      , Filter (PDFName "FlateDecode")     PDFNull
      ]
      . fcBytes
