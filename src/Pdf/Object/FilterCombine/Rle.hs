module Pdf.Object.FilterCombine.Rle
  ( rle
  ) where

import Codec.Compression.RunLength qualified as RL

import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Functor ((<&>))

import Pdf.Object.Container (Filter (Filter))
import Pdf.Object.FilterCombine.FilterCombination
    ( FilterCombination
    , mkFCAppend
    )
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))

rle
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Fallible FilterCombination
rle _ stream =
  RL.compress stream <&> mkFCAppend [Filter (PDFName "RunLengthDecode") PDFNull]
