{-|
Predictor + RLE + Zopfli/Deflate filter combination.

Applies PNG predictor tuned for RLE entropy, stores then RLE-compresses, and
finally compresses with either Zopfli or fast Deflate, producing a
`FilterCombination` with `FlateDecode`, `RunLengthDecode`, and a second
`FlateDecode` carrying predictor parameters.
-}
module PDF.Processing.FilterCombine.PredRleCompressor
  ( predRleCompressor
  , predRleCompressorFromPredicted
  , predRleEntropies
  ) where

import Codec.Compression.BrotliForPDF qualified as BR
import Codec.Compression.ECT qualified as ECT
import Codec.Compression.Flate qualified as FL
import Codec.Compression.Predict
  (Entropy (EntropyRLE), Predictor (PNGOptimum), predictPNGVariants)
import Codec.Compression.Predict.Entropy (Entropy (EntropyDeflate, EntropyMSAD))
import Codec.Compression.RunLength qualified as RL

import Data.Bitmap.BitmapConfiguration
  (BitmapConfiguration (bcBitsPerComponent, bcComponents, bcLineWidth))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Functor ((<&>))
import Data.List (minimumBy)
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName, PDFNull), mkPDFDictionary)
import Data.PDF.Settings
  (UseCompressor (UseBrotli, UseDeflate, UseECT, UseZopfli))
import Data.UnifiedError (UnifiedError (InvalidFilterParm))

import PDF.Object.Object.ToPDFNumber (mkPDFNumber)

getCompressor :: UseCompressor -> (ByteString -> Fallible ByteString, PDFObject)
getCompressor UseZopfli  = (FL.compress    , PDFName "FlateDecode" )
getCompressor UseDeflate = (FL.fastCompress, PDFName "FlateDecode" )
getCompressor UseBrotli  = (BR.compress    , PDFName "BrotliDecode")
getCompressor UseECT     = (ECT.compress   , PDFName "FlateDecode" )

{-|
Apply RLE-tuned predictor pipeline: store → RLE → Zopfli/Deflate.

Requires `(width, components)`; returns `InvalidFilterParm` when width is
missing.
-}
predRleCompressor
  :: Maybe BitmapConfiguration
  -> ByteString
  -> UseCompressor
  -> Fallible FilterCombination
predRleCompressor (Just bitmapConfig) stream useCompressor = do
  predicted <- predictPNGVariants [ (entropy, PNGOptimum)
                                  | entropy <- predRleEntropies
                                  ]
                                  bitmapConfig
                                  stream

  predRleCompressorFromPredicted bitmapConfig predicted useCompressor

predRleCompressor _noBitmapConfig _stream _useCompressor = Left
  $ InvalidFilterParm "no width given to predRleCompressor"

-- | Keep strategy order stable when compressed sizes tie.
predRleEntropies :: [Entropy]
predRleEntropies = [EntropyDeflate, EntropyMSAD, EntropyRLE]

-- | Compress precomputed PNG streams, allowing callers to share prediction work.
predRleCompressorFromPredicted
  :: BitmapConfiguration
  -> [ByteString]
  -> UseCompressor
  -> Fallible FilterCombination
predRleCompressorFromPredicted _ [] _ = Left
  $ InvalidFilterParm "no predicted streams given to predRleCompressor"

predRleCompressorFromPredicted bitmapConfig predicted useCompressor = do
  let
    compressor :: ByteString -> Fallible ByteString
    filterName :: PDFObject
    (compressor, filterName) = getCompressor useCompressor

    width :: Int
    width = bcLineWidth bitmapConfig

    components :: Int
    components = bcComponents bitmapConfig

  compressed <- mapM (\stream -> FL.noCompress stream
                             >>= RL.compress
                             >>= compressor
                     )
                     predicted
                <&> minimumBy (\a b -> BS.length a `compare` BS.length b)

  return $ mkFCAppend
    [ Filter filterName PDFNull
    , Filter (PDFName "RunLengthDecode") PDFNull
    , Filter
      (PDFName "FlateDecode")
      (mkPDFDictionary
        [ ("Predictor", mkPDFNumber PNGOptimum)
        , ("Columns"  , mkPDFNumber width)
        , ("Colors"   , mkPDFNumber components)
        , ( "BitsPerComponent"
          , mkPDFNumber . fromEnum $ bcBitsPerComponent bitmapConfig
          )
        ]
      )
    ]
    compressed
