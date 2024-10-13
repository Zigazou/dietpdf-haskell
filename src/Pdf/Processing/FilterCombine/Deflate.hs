module Pdf.Processing.FilterCombine.Deflate
  ( deflate
  ) where

import Codec.Compression.Flate qualified as FL

import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Functor ((<&>))

import Pdf.Object.Container (Filter (Filter))
import Pdf.Processing.FilterCombine.FilterCombination
    ( FilterCombination
    , mkFCAppend
    )
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))

deflate
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Fallible FilterCombination
deflate _ stream =
  FL.fastCompress stream <&> mkFCAppend [Filter (PDFName "FlateDecode") PDFNull]
