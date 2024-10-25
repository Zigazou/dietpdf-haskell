module Codec.Compression.PredictSpec
  ( spec
  ) where

import Codec.Compression.Predict
    ( Entropy (EntropyShannon)
    , Predictor (PNGAverage, PNGNone, PNGSub, PNGUp, TIFFNoPrediction)
    , predict
    , unpredict
    )
import Codec.Compression.Predict.Entropy (entropyShannon)

import Control.Monad (forM_)

import Data.ByteString (ByteString)

import Test.Hspec (Spec, describe, it, shouldBe)

predictorExamples :: [((Predictor, Int, Int), (ByteString, ByteString))]
predictorExamples =
  [ ((PNGNone, 1, 1), ("\xAA", "\x00\xAA"))
  , ((PNGNone, 1, 1), ("\xAA\xAA\xAA", "\x00\xAA\x00\xAA\x00\xAA"))
  , ((PNGNone, 1, 3), ("\xAA\xAA\xAA", "\x00\xAA\xAA\xAA"))
  , ((PNGSub, 4, 1) , ("\x10\x20\x30\x40", "\x01\x10\x10\x10\x10"))
  , ((PNGSub, 2, 1) , ("\x10\x20\x30\x40", "\x01\x10\x10\x01\x30\x10"))
  , ((PNGSub, 1, 2) , ("\x10\x20\x30\x40", "\x01\x10\x20\x01\x30\x40"))
  , ((PNGSub, 2, 2) , ("\x10\x20\x30\x40", "\x01\x10\x20\x20\x20"))
  , ((PNGUp, 4, 1)  , ("\x01\x02\x03\x04", "\x02\x01\x02\x03\x04"))
  , ((PNGUp, 1, 2)  , ("\x01\x02\x03\x04", "\x02\x01\x02\x02\x02\x02"))
  , ((PNGUp, 1, 1), ("\x01\x02\x03\x04", "\x02\x01\x02\x01\x02\x01\x02\x01"))
  , ((PNGUp, 2, 1)  , ("\x01\x02\x02\x03", "\x02\x01\x02\x02\x01\x01"))
  , ( (PNGAverage, 3, 1)
    , ("\x01\x02\x03\x04\x05\x06", "\x03\x01\x02\x02\x03\x04\x02\x02")
    )
  , ((TIFFNoPrediction, 1, 1), ("\xAA", "\xAA"))
  , ((TIFFNoPrediction, 2, 1), ("\x10\x20\x30\x40", "\x10\x20\x30\x40"))
  ]

spec :: Spec
spec = do
  describe "predict"
    $ forM_ predictorExamples
    $ \((predictor, width, components), (example, expected)) ->
        it
            (  "should work with "
            ++ show predictor
            ++ " "
            ++ show components
            ++ " "
            ++ show example
            )
          $          predict EntropyShannon predictor width components example
          `shouldBe` Right expected

  describe "unpredict"
    $ forM_ predictorExamples
    $ \((predictor, width, components), (expected, example)) ->
        it
            (  "should work with "
            ++ show predictor
            ++ " "
            ++ show components
            ++ " "
            ++ show example
            )
          $          unpredict predictor width components example
          `shouldBe` Right expected

  describe "entropy"
    $          it "should find 1.8464... for \"1223334444\""
    $          entropyShannon "1223334444"
    `shouldBe` 1.8464393446710154
