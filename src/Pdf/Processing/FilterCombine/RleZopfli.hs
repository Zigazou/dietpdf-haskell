module Pdf.Processing.FilterCombine.RleZopfli
  ( rleZopfli
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

rleZopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Fallible FilterCombination
rleZopfli _ stream = do
  rle Nothing stream
    >>= FL.compress . fcBytes
    <&> mkFCAppend
      [ Filter (PDFName "FlateDecode")     PDFNull
      , Filter (PDFName "RunLengthDecode") PDFNull
      ]
