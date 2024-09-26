module Pdf.Object.FilterCombine.RleZopfli
  ( rleZopfli
  ) where

import Codec.Compression.Flate qualified as FL

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

rleZopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError FilterCombination
rleZopfli _ stream = do
  rle Nothing stream
    >>= FL.compress . fcBytes
    <&> mkFCAppend
      [ Filter (PDFName "FlateDecode")     PDFNull
      , Filter (PDFName "RunLengthDecode") PDFNull
      ]
