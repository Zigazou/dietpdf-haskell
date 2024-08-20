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
                                                , entropyShannon
                                                , EntropyType(EntropyShannon)
                                                , Predictor
                                                  ( TIFFNoPrediction
                                                  , PNGNone
                                                  , PNGSub
                                                  , PNGUp
                                                  , PNGAverage
                                                  )
                                                )

predictorExamples :: [(EntropyType, (Predictor, Int, Int), (BS.ByteString, BS.ByteString))]
predictorExamples =
  [ (EntropyShannon, (PNGNone, 1, 1), ("\xAA", "\x00\xAA"))
  , (EntropyShannon, (PNGNone, 1, 1), ("\xAA\xAA\xAA", "\x00\xAA\x00\xAA\x00\xAA"))
  , (EntropyShannon, (PNGNone, 1, 3), ("\xAA\xAA\xAA", "\x00\xAA\xAA\xAA"))
  , (EntropyShannon, (PNGSub, 4, 1) , ("\x10\x20\x30\x40", "\x01\x10\x10\x10\x10"))
  , (EntropyShannon, (PNGSub, 2, 1) , ("\x10\x20\x30\x40", "\x01\x10\x10\x01\x30\x10"))
  , (EntropyShannon, (PNGSub, 1, 2) , ("\x10\x20\x30\x40", "\x01\x10\x20\x01\x30\x40"))
  , (EntropyShannon, (PNGSub, 2, 2) , ("\x10\x20\x30\x40", "\x01\x10\x20\x20\x20"))
  , (EntropyShannon, (PNGUp, 4, 1)  , ("\x01\x02\x03\x04", "\x02\x01\x02\x03\x04"))
  , (EntropyShannon, (PNGUp, 1, 2)  , ("\x01\x02\x03\x04", "\x02\x01\x02\x02\x02\x02"))
  , (EntropyShannon, (PNGUp, 1, 1), ("\x01\x02\x03\x04", "\x02\x01\x02\x01\x02\x01\x02\x01"))
  , (EntropyShannon, (PNGUp, 2, 1)  , ("\x01\x02\x02\x03", "\x02\x01\x02\x02\x01\x01"))
  , (EntropyShannon, (PNGAverage, 3, 1)
    , ("\x01\x02\x03\x04\x05\x06", "\x03\x01\x02\x02\x03\x04\x02\x02")
    )
  , (EntropyShannon, (TIFFNoPrediction, 1, 1), ("\xAA", "\xAA"))
  , (EntropyShannon, (TIFFNoPrediction, 2, 1), ("\x10\x20\x30\x40", "\x10\x20\x30\x40"))
  ]

spec :: Spec
spec = do
  describe "predict"
    $ forM_ predictorExamples
    $ \(entropy, (predictor, width, components), (example, expected)) ->
        it
            (  "should work with "
            ++ show predictor
            ++ " "
            ++ show components
            ++ " "
            ++ show example
            )
<<<<<<< HEAD
          $          predict EntropyShannon predictor width components example
=======
          $          predict entropy predictor width components example
>>>>>>> 22fc58f3461979b1cfb82cd647a2b8666e317fb7
          `shouldBe` Right expected

  describe "unpredict"
    $ forM_ predictorExamples
    $ \(_entropy, (predictor, width, components), (expected, example)) ->
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

  describe "entropyShannon"
    $          it "should find 1.8464... for \"1223334444\""
    $          entropyShannon "1223334444"
    `shouldBe` 1.8464393446710154
