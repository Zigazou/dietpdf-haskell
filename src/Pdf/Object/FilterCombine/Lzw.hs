module Pdf.Object.FilterCombine.Lzw
  ( lzw
  ) where

import Codec.Compression.LZW qualified as LZW

import Data.ByteString qualified as BS
import Data.Functor ((<&>))

import Pdf.Object.Container (Filter (Filter))
import Pdf.Object.FilterCombine.FilterCombination
    ( FilterCombination
    , mkFCAppend
    )
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))

import Util.UnifiedError (UnifiedError)

lzw
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError FilterCombination
lzw _ stream =
  LZW.compress stream <&> mkFCAppend [Filter (PDFName "LZWDecode") PDFNull]
