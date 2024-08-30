module Pdf.Object.FilterCombine.ZopfliRle
  ( zopfliRle
  ) where

import Codec.Compression.Flate qualified as FL

import Data.ByteString qualified as BS

import Pdf.Object.Container (Filter (Filter), FilterList)
import Pdf.Object.FilterCombine.Rle (rle)
import Pdf.Object.Object (PDFObject (PDFName, PDFNull))

import Util.Array (mkArray)
import Util.UnifiedError (UnifiedError)

zopfliRle
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError (FilterList, BS.ByteString)
zopfliRle _ stream = do
  compressed <- FL.compress stream >>= rle Nothing
  return
    ( mkArray
      [ Filter (PDFName "RunLengthDecode") PDFNull
      , Filter (PDFName "FlateDecode")     PDFNull
      ]
    , snd compressed
    )
