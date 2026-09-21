module PDF.Processing.FilterCombine.PredCompressorSpec (spec) where

import Control.Monad (forM_)
import Data.Bitmap.BitmapConfiguration (BitmapConfiguration (BitmapConfiguration))
import Data.Bitmap.BitsPerComponent (BitsPerComponent (BC16Bits, BC2Bits, BC4Bits, BC8Bits))
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.PDF.Filter (Filter (fDecodeParms))
import Data.PDF.FilterCombination (FilterCombination (fcBytes, fcList))
import Data.PDF.PDFObject (PDFObject (PDFIndirectObjectWithStream, PDFNumber))
import Data.PDF.PDFWork (evalPDFWorkT)
import Data.PDF.Settings (UseCompressor (UseDeflate))
import PDF.Object.Container (setFilters)
import PDF.Object.Object.Properties (getValueForKey)
import PDF.Object.State (getStream)
import PDF.Processing.FilterCombine.PredCompressor (predCompressor)
import PDF.Processing.FilterCombine.PredRleCompressor (predRleCompressor)
import PDF.Processing.Unfilter (unfilter)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

right :: Show e => Either e a -> IO a
right = either (fail . show) pure

spec :: Spec
spec = describe "predCompressor" $ do
  -- A smooth 16-bit tint table strongly favors TIFF prediction, but Poppler's
  -- TIFF decoder truncates reconstructed 16-bit samples to eight bits.
  let samples = BS.pack $ concat
        [ [fromIntegral (value `div` 256), fromIntegral value, 0, 0,
           fromIntegral (value `div` 512), fromIntegral (value `div` 2), 0, 0]
        | index <- [0..511 :: Int], let value = index * 105
        ]
  forM_ [Nothing, Just (BitmapConfiguration 512 4 BC16Bits)] $ \config ->
    it ("avoids 16-bit TIFF prediction for " ++ show config) $ do
      encoded <- right (predCompressor config samples UseDeflate)
      forM_ (toList (fcList encoded)) $ \flt -> do
        let params = fDecodeParms flt
        (getValueForKey "Predictor" params, getValueForKey "BitsPerComponent" params)
          `shouldNotBe` (Just (PDFNumber 2), Just (PDFNumber 16))
      object <- evalPDFWorkT (setFilters (fcList encoded)
        (PDFIndirectObjectWithStream 1 0 mempty (fcBytes encoded))) >>= right
      decoded <- evalPDFWorkT (unfilter object >>= getStream) >>= right
      decoded `shouldBe` samples

  forM_ [("predictor", predCompressor), ("predictor and RLE", predRleCompressor)] $ \(name, encode) ->
    forM_ [BC2Bits, BC4Bits, BC8Bits, BC16Bits] $ \bits ->
      it ("preserves " ++ show bits ++ " samples through " ++ name) $ do
        let original = BS.pack [0..63]
        encoded <- right (encode (Just (BitmapConfiguration 16 1 bits)) original UseDeflate)
        forM_ (toList (fcList encoded)) $ \flt -> do
          let params = fDecodeParms flt
          case getValueForKey "Predictor" params of
            Nothing -> pure ()
            Just _ -> getValueForKey "BitsPerComponent" params
              `shouldBe` Just (PDFNumber (fromIntegral (fromEnum bits)))
        object <- evalPDFWorkT (setFilters (fcList encoded)
          (PDFIndirectObjectWithStream 1 0 mempty (fcBytes encoded))) >>= right
        decoded <- evalPDFWorkT (unfilter object >>= getStream) >>= right
        decoded `shouldBe` original
