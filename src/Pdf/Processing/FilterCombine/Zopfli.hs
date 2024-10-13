module Pdf.Processing.FilterCombine.Zopfli
  ( zopfli
  ) where

import Codec.Compression.Flate qualified as FL

import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Functor ((<&>))

import Pdf.Object.Container (Filter (Filter))
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))
import Pdf.Processing.FilterCombine.FilterCombination
    ( FilterCombination
    , mkFCAppend
    )

zopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Fallible FilterCombination
zopfli _ stream =
  FL.compress stream <&> mkFCAppend [Filter (PDFName "FlateDecode") PDFNull]
