module Pdf.Object.FilterCombine.ZopfliRle
  ( zopfliRle
  ) where

import Codec.Compression.Flate qualified as FL

import Data.ByteString qualified as BS
import Data.Functor ((<&>))

import Pdf.Object.Container (Filter (Filter))
import Pdf.Object.FilterCombine.FilterCombination
    ( FilterCombination
    , fcBytes
    , mkFCAppend
    )
import Pdf.Object.FilterCombine.Rle (rle)
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))

import Util.UnifiedError (UnifiedError)

zopfliRle
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError FilterCombination
zopfliRle _ stream =
  FL.compress stream
    >>= rle Nothing
    <&> mkFCAppend
      [ Filter (PDFName "RunLengthDecode") PDFNull
      , Filter (PDFName "FlateDecode")     PDFNull
      ]
      . fcBytes