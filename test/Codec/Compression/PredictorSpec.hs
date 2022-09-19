{-# LANGUAGE OverloadedStrings #-}
module Codec.Compression.PredictorSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Data.ByteString               as BS
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           Codec.Compression.Predictor    ( predict
                                                , unpredict
                                                , Predictor
                                                  ( TIFFNoPrediction
                                                  , TIFFPredictor2
                                                  , PNGNone
                                                  , PNGSub
                                                  , PNGUp
                                                  , PNGAverage
                                                  , PNGPaeth
                                                  , PNGOptimum
                                                  )
                                                )

predictorExamples :: [(Predictor, Int, BS.ByteString, BS.ByteString)]
predictorExamples =
  [ (PNGNone, 1, "\xAA"            , "\x0A\xAA")
  , (PNGNone, 1, "\xAA\xAA\xAA"    , "\x0A\xAA\x0A\xAA\x0A\xAA")
  , (PNGSub , 4, "\x10\x20\x30\x40", "\x0B\x10\x10\x10\x10")
  , (PNGSub , 2, "\x10\x20\x30\x40", "\x0B\x10\x10\x0B\x30\x10")
  , (PNGUp  , 4, "\x01\x02\x03\x04", "\x0C\x01\x02\x03\x04")
  , (PNGUp  , 1, "\x01\x02\x03\x04", "\x0C\x01\x0C\x01\x0C\x01\x0C\x01")
  , (PNGUp  , 2, "\x01\x02\x02\x03", "\x0C\x01\x02\x0C\x01\x01")
  , ( PNGAverage
    , 3
    , "\x01\x02\x03\x04\x05\x06"
    , "\x0D\x01\x02\x02\x0D\x04\x02\x02"
    )
  , (TIFFNoPrediction, 1, "\xAA"            , "\xAA")
  , (TIFFNoPrediction, 2, "\x10\x20\x30\x40", "\x10\x20\x30\x40")
  ]

spec :: Spec
spec = do
  describe "predict"
    $ forM_ predictorExamples
    $ \(predictor, width, example, expected) ->
        it ("should work with " ++ show predictor ++ " " ++ show example)
          $          predict predictor width example
          `shouldBe` Right expected

  describe "unpredict"
    $ forM_ predictorExamples
    $ \(predictor, width, expected, example) ->
        it ("should work with " ++ show predictor ++ " " ++ show example)
          $          unpredict predictor width example
          `shouldBe` Right expected
