module Pdf.Object.FilterCombine.Rle
  ( rle
  ) where

import qualified Codec.Compression.RunLength   as RL
import qualified Data.ByteString               as BS
import           Pdf.Object.Container           ( Filter(Filter)
                                                , FilterList
                                                )
import           Pdf.Object.Object              ( PDFObject(PDFName, PDFNull) )
import           Util.UnifiedError              ( UnifiedError )
import           Util.Array                     ( mkArray )

rle
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError (FilterList, BS.ByteString)
rle _ stream = do
  compressed <- RL.compress stream
  return (mkArray [Filter (PDFName "RunLengthDecode") PDFNull], compressed)
