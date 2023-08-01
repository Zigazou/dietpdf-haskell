module Pdf.Object.FilterCombine.RleZopfli
  ( rleZopfli
  ) where

import qualified Codec.Compression.Flate       as FL
import qualified Data.ByteString               as BS
import           Pdf.Object.Container           ( Filter(Filter)
                                                , FilterList
                                                )
import           Pdf.Object.Object              ( PDFObject(PDFName, PDFNull) )
import           Util.UnifiedError              ( UnifiedError )
import           Util.Array                     ( mkArray )
import           Pdf.Object.FilterCombine.Rle   ( rle )

rleZopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError (FilterList, BS.ByteString)
rleZopfli _ stream = do
  compressed <- rle Nothing stream >>= FL.compress . snd
  return
    ( mkArray
      [ Filter (PDFName "FlateDecode")     PDFNull
      , Filter (PDFName "RunLengthDecode") PDFNull
      ]
    , compressed
    )
