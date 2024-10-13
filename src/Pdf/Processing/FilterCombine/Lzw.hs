module Pdf.Processing.FilterCombine.Lzw
  ( lzw
  ) where

import Codec.Compression.LZW qualified as LZW

import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Functor ((<&>))

import Pdf.Object.Container (Filter (Filter))
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))
import Pdf.Processing.FilterCombine.FilterCombination
    ( FilterCombination
    , mkFCAppend
    )

lzw
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Fallible FilterCombination
lzw _ stream =
  LZW.compress stream <&> mkFCAppend [Filter (PDFName "LZWDecode") PDFNull]
