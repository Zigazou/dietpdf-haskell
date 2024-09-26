module Pdf.Object.FilterCombine.Zopfli
  ( zopfli
  ) where

import Codec.Compression.Flate qualified as FL

import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.UnifiedError (UnifiedError)

import Pdf.Object.Container (Filter (Filter))
import Pdf.Object.FilterCombine.FilterCombination
    ( FilterCombination
    , mkFCAppend
    )
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))

zopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError FilterCombination
zopfli _ stream =
  FL.compress stream <&> mkFCAppend [Filter (PDFName "FlateDecode") PDFNull]
