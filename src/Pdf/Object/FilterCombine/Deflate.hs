module Pdf.Object.FilterCombine.Deflate
  ( deflate
  ) where

import qualified Codec.Compression.Flate       as FL
import qualified Data.ByteString               as BS
import           Pdf.Object.Container           ( Filter(Filter)
                                                , FilterList
                                                )
import           Pdf.Object.Object              ( PDFObject(PDFName, PDFNull) )
import           Util.UnifiedError              ( UnifiedError )
import           Util.Array                     ( mkArray )

deflate
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError (FilterList, BS.ByteString)
deflate _ stream = do
  compressed <- FL.fastCompress stream
  return (mkArray [Filter (PDFName "FlateDecode") PDFNull], compressed)
