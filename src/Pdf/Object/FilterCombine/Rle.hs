module Pdf.Object.FilterCombine.Rle
  ( rle
  ) where

import Codec.Compression.RunLength qualified as RL

import Data.ByteString qualified as BS
import Data.Functor ((<&>))

import Pdf.Object.Container (Filter (Filter))
import Pdf.Object.FilterCombine.FilterCombination
    ( FilterCombination
    , mkFCAppend
    )
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))

import Util.UnifiedError (UnifiedError)

rle
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError FilterCombination
rle _ stream =
  RL.compress stream <&> mkFCAppend [Filter (PDFName "RunLengthDecode") PDFNull]
