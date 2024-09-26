module Pdf.Object.FilterCombine.Lzw
  ( lzw
  ) where

import Codec.Compression.LZW qualified as LZW

import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Functor ((<&>))

import Pdf.Object.Container (Filter (Filter))
import Pdf.Object.FilterCombine.FilterCombination
    ( FilterCombination
    , mkFCAppend
    )
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))

lzw
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Fallible FilterCombination
lzw _ stream =
  LZW.compress stream <&> mkFCAppend [Filter (PDFName "LZWDecode") PDFNull]
