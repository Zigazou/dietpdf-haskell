module Codec.Compression.PredictSpec
  ( spec
  ) where

import Codec.Compression.Predict
  ( Entropy (EntropyShannon)
  , Predictor (PNGAverage, PNGNone, PNGSub, PNGUp, TIFFNoPrediction, TIFFPredictor2)
  , predict
  , unpredict
  )
import Codec.Compression.Predict.Entropy (entropyShannon)

import Control.Monad (forM_, replicateM)

import Data.Bitmap.BitmapConfiguration
  (BitmapConfiguration (BitmapConfiguration), bitmapRawWidth)
import Data.Bitmap.BitsPerComponent
  (BitsPerComponent (BC16Bits, BC2Bits, BC4Bits, BC8Bits))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Functor ((<&>))
import Data.Word (Word8)

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, arbitrary, elements, forAll)

import Util.ByteString (HexBS (HexBS))

randomBitmapConfig :: Gen BitmapConfiguration
randomBitmapConfig = do
  lineWidth  <- elements [1..32]
  components <- elements [1, 2, 3, 4]
  bpc        <- elements [BC2Bits, BC4Bits, BC8Bits, BC16Bits]
  return $ BitmapConfiguration lineWidth components bpc

randomString :: Int -> Gen ByteString
randomString len = do
  replicateM len (arbitrary :: Gen Word8) <&> BS.pack

predictorExamples :: [((Predictor, BitmapConfiguration), (ByteString, ByteString))]
predictorExamples =
  [ ( (PNGNone, BitmapConfiguration 1 1 BC8Bits)
    , ("\xAA", "\x00\xAA"))
  , ( (PNGNone, BitmapConfiguration 1 1 BC8Bits)
    , ("\xAA\xAA\xAA", "\x00\xAA\x00\xAA\x00\xAA"))
  , ( (PNGNone, BitmapConfiguration 1 3 BC8Bits)
    , ("\xAA\xAA\xAA", "\x00\xAA\xAA\xAA"))
  , ( (PNGSub, BitmapConfiguration 4 1 BC8Bits)
    , ("\x10\x20\x30\x40", "\x01\x10\x10\x10\x10"))
  , ( (PNGSub, BitmapConfiguration 2 1 BC8Bits)
    , ("\x10\x20\x30\x40", "\x01\x10\x10\x01\x30\x10"))
  , ( (PNGSub, BitmapConfiguration 1 2 BC8Bits)
    , ("\x10\x20\x30\x40", "\x01\x10\x20\x01\x30\x40"))
  , ( (PNGSub, BitmapConfiguration 2 2 BC8Bits)
    , ("\x10\x20\x30\x40", "\x01\x10\x20\x20\x20"))
  , ( (PNGUp, BitmapConfiguration 4 1 BC8Bits)
    , ("\x01\x02\x03\x04", "\x02\x01\x02\x03\x04"))
  , ( (PNGUp, BitmapConfiguration 1 2 BC8Bits)
    , ("\x01\x02\x03\x04", "\x02\x01\x02\x02\x02\x02"))
  , ( (PNGUp, BitmapConfiguration 1 1 BC8Bits)
    , ("\x01\x02\x03\x04", "\x02\x01\x02\x01\x02\x01\x02\x01"))
  , ( (PNGUp, BitmapConfiguration 2 1 BC8Bits)
    , ("\x01\x02\x02\x03", "\x02\x01\x02\x02\x01\x01"))
  , ( (PNGAverage, BitmapConfiguration 3 1 BC8Bits)
    , ("\x01\x02\x03\x04\x05\x06", "\x03\x01\x02\x02\x03\x04\x02\x02")
    )

    -- TIFF no prediction
  , ( (TIFFNoPrediction, BitmapConfiguration 1 1 BC8Bits)
    , ("\xAA", "\xAA"))
  , ( (TIFFNoPrediction, BitmapConfiguration 2 1 BC8Bits)
    , ("\x10\x20\x30\x40", "\x10\x20\x30\x40"))

    -- TIFF predictor 2, 8 bits per component
  , ( (TIFFPredictor2, BitmapConfiguration 4 1 BC8Bits)
    , ("\x10\x20\x30\x40", "\x10\x10\x10\x10"))

    -- TIFF predictor 2, 16 bits per component
  , ( (TIFFPredictor2, BitmapConfiguration 4 1 BC16Bits)
    , ( "\x10\x23\x20\x23\x30\x23\x40\x23"
      , "\x10\x23\x10\x00\x10\x00\x10\x00"
      )
    )
  , ( (TIFFPredictor2, BitmapConfiguration 2 2 BC16Bits)
    , ( "\x11\x22\x33\x44\x55\x66\x77\x88"
      , "\x11\x22\x33\x44\x44\x44\x44\x44"
      )
    )
  , ( (TIFFPredictor2, BitmapConfiguration 1 4 BC16Bits)
    , ( "\x11\x22\x33\x44\x55\x66\x77\x88"
      , "\x11\x22\x33\x44\x55\x66\x77\x88"
      )
    )
  , ( (TIFFPredictor2, BitmapConfiguration 2 4 BC16Bits)
    , ( "\x11\x22\x33\x44\x55\x66\x77\x88\x11\x22\x33\x44\x55\x66\x77\x88"
      , "\x11\x22\x33\x44\x55\x66\x77\x88\x00\x00\x00\x00\x00\x00\x00\x00"
      )
    )
  , ( (TIFFPredictor2, BitmapConfiguration 2 4 BC16Bits)
    , ( "\x01\x02\x02\x02\x02\x02\x02\x02\x00\x01\x01\x02\x01\x01\x01\x00"
      , "\x01\x02\x02\x02\x02\x02\x02\x02\xfe\xff\xff\x00\xfe\xff\xfe\xfe"
      )
    )

    -- TIFF predictor 2, 4 bits per component
  , ( (TIFFPredictor2, BitmapConfiguration 8 1 BC4Bits)
    , ("\x11\x22\x33\x44", "\x10\x10\x10\x10"))
  , ( (TIFFPredictor2, BitmapConfiguration 1 3 BC4Bits)
    , ("\x12\x30", "\x12\x30"))
  , ( (TIFFPredictor2, BitmapConfiguration 2 3 BC4Bits)
    , ("\x12\x34\x56", "\x12\x33\x33"))
  , ( (TIFFPredictor2, BitmapConfiguration 3 3 BC4Bits)
    , ("\x12\x34\x56\x78\x90", "\x12\x33\x33\x33\x30"))

    -- TIFF predictor 2, 2 bits per component
  , ( (TIFFPredictor2, BitmapConfiguration 16 1 BC2Bits)
    , ("\x11\x22\x33\x44", "\x1d\xea\xb7\xb7"))
  , ( (TIFFPredictor2, BitmapConfiguration 15 1 BC2Bits)
    , ("\x11\x22\x33\x40", "\x1d\xea\xb7\xb0"))
  , ( (TIFFPredictor2, BitmapConfiguration 14 3 BC2Bits)
    , ( "\x07\x0a\x0b\x07\x00\x05\x0b\x09\x0d\x0d\x0a"
      , "\x07\x3e\x23\x2b\x34\x05\x37\x2d\x29\x19\x1a"
      )
    )
  ]

spec :: Spec
spec = do
  describe "predict"
    $ forM_ predictorExamples
    $ \((predictor, bitmapConfig), (example, expected)) ->
        it
            (  "should work with "
            ++ show predictor
            ++ " "
            ++ show bitmapConfig
            ++ " "
            ++ show (HexBS example)
            )
          $          HexBS <$> predict EntropyShannon predictor bitmapConfig example
          `shouldBe` HexBS <$> Right expected

  describe "unpredict"
    $ forM_ predictorExamples
    $ \((predictor, bitmapConfig), (expected, example)) ->
        it
            (  "should work with "
            ++ show predictor
            ++ " "
            ++ show bitmapConfig
            ++ " "
            ++ show (HexBS example)
            )
          $          HexBS <$> unpredict predictor bitmapConfig example
          `shouldBe` HexBS <$> Right expected

  describe "entropy"
    $          it "should find 1.8464... for \"1223334444\""
    $          entropyShannon "1223334444"
    `shouldBe` 1.8464393446710154

  it "predict then unpredict should give the same value"
    $ forAll randomBitmapConfig
    $ \bitmapConfig -> do
        forAll (randomString (bitmapRawWidth bitmapConfig)) $ \originalBytes -> do
          let
            predicted = predict EntropyShannon TIFFPredictor2 bitmapConfig originalBytes
            unpredicted = predicted >>= unpredict TIFFPredictor2 bitmapConfig

          HexBS <$> unpredicted `shouldBe` HexBS <$> Right originalBytes
