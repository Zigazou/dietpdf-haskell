{-|
This module implements the LZW uncompress alfgorithm as used in PDF file
(and also TIFF)
-}
module Codec.Compression.LZW
  ( decompress
  , compress
  ) where

import Codec.Compression.LZW.LZWDecode (decompress)
import Codec.Compression.LZW.LZWEncode (compress)
