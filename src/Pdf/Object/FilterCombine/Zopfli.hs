module Pdf.Object.FilterCombine.Zopfli
  ( zopfli
  ) where

import qualified Codec.Compression.Flate       as FL
import qualified Data.ByteString               as BS
import           Pdf.Object.Container           ( Filter(Filter)
                                                , FilterList
                                                )
import           Pdf.Object.Object              ( PDFObject(PDFName, PDFNull) )
import           Util.UnifiedError              ( UnifiedError )
import           Util.Array                     ( mkArray )

zopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError (FilterList, BS.ByteString)
zopfli _ stream = do
  compressed <- FL.compress stream
  return (mkArray [Filter (PDFName "FlateDecode") PDFNull], compressed)
