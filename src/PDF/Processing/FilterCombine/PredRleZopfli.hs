{-|
Predictor + RLE + Zopfli/Deflate filter combination.

Applies PNG predictor tuned for RLE entropy, stores then RLE-compresses, and
finally compresses with either Zopfli or fast Deflate, producing a
`FilterCombination` with `FlateDecode`, `RunLengthDecode`, and a second
`FlateDecode` carrying predictor parameters.
-}
module PDF.Processing.FilterCombine.PredRleZopfli
  ( predRleZopfli
  ) where

import Codec.Compression.Flate qualified as FL
import Codec.Compression.Predict
  (Entropy (EntropyRLE), Predictor (PNGOptimum), predict)
import Codec.Compression.Predict.Entropy (Entropy (EntropyDeflate))
import Codec.Compression.RunLength qualified as RL

import Data.Bitmap.BitmapConfiguration
  (BitmapConfiguration (bcComponents, bcLineWidth))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.Functor ((<&>))
import Data.List (minimumBy)
import Data.PDF.Filter (Filter (Filter))
import Data.PDF.FilterCombination (FilterCombination, mkFCAppend)
import Data.PDF.PDFObject (PDFObject (PDFName, PDFNull), mkPDFDictionary)
import Data.PDF.Settings (UseZopfli (UseDeflate, UseZopfli))
import Data.UnifiedError (UnifiedError (InvalidFilterParm))

import PDF.Object.Object.ToPDFNumber (mkPDFNumber)

getCompressor :: UseZopfli -> (ByteString -> Fallible ByteString)
getCompressor UseZopfli  = FL.compress
getCompressor UseDeflate = FL.fastCompress

{-|
Apply RLE-tuned predictor pipeline: store → RLE → Zopfli/Deflate.

Requires `(width, components)`; returns `InvalidFilterParm` when width is
missing.
-}
predRleZopfli
  :: Maybe BitmapConfiguration
  -> ByteString
  -> UseZopfli
  -> Fallible FilterCombination
predRleZopfli (Just bitmapConfig) stream useZopfli = do
  let
    compressor = getCompressor useZopfli
    entropies = [EntropyDeflate, EntropyRLE]
    width     = bcLineWidth bitmapConfig
    components = bcComponents bitmapConfig

  -- Try all entropies and select the best compressed result.
  compressed <- mapM (\entropy ->
                        predict entropy PNGOptimum bitmapConfig stream
                        >>= FL.noCompress
                        >>= RL.compress
                        >>= compressor
                      ) entropies
                <&> minimumBy (\a b -> BS.length a `compare` BS.length b)

  return $ mkFCAppend
    [ Filter (PDFName "FlateDecode")     PDFNull
    , Filter (PDFName "RunLengthDecode") PDFNull
    , Filter
      (PDFName "FlateDecode")
      (mkPDFDictionary
        [ ("Predictor", mkPDFNumber PNGOptimum)
        , ("Columns"  , mkPDFNumber width)
        , ("Colors"   , mkPDFNumber components)
        ]
      )
    ]
    compressed

predRleZopfli _noBitmapConfig _stream _useZopfli = Left
  $ InvalidFilterParm "no width given to predRleZopfli"
