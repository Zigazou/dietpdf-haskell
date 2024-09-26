module Pdf.Object.FilterCombine.Deflate
  ( deflate
  ) where

import Codec.Compression.Flate qualified as FL

import Data.ByteString qualified as BS
import Data.Functor ((<&>))

import Pdf.Object.Container (Filter (Filter))
import Pdf.Object.FilterCombine.FilterCombination
    ( FilterCombination
    , mkFCAppend
    )
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))

import Data.UnifiedError (UnifiedError)

deflate
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError FilterCombination
deflate _ stream =
  FL.fastCompress stream <&> mkFCAppend [Filter (PDFName "FlateDecode") PDFNull]
