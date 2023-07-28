module Util.NumberSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Data.ByteString               as BS
import           Util.Number                    ( toNumber
                                                , fromNumber
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

toNumberExamples :: [(BS.ByteString, Int)]
toNumberExamples =
  [ (""          , 0)
  , ("0"         , 0)
  , ("0000"      , 0)
  , ("1"         , 1)
  , ("01"        , 1)
  , ("00001"     , 1)
  , ("1234"      , 1234)
  , ("4873987128", 4873987128)
  ]

fromNumberExamples :: [(Double, BS.ByteString)]
fromNumberExamples =
  [ (1.0    , "1")
  , (0.0    , "0")
  , (-0.0   , "0")
  , (0.1    , ".1")
  , (-0.1   , "-.1")
  , (0.00028, ".00028")
  ]

spec :: Spec
spec = describe "toNumber" $ do
  forM_ toNumberExamples $ \(example, expected) ->
    it ("should decode bytestring " ++ show example)
      $          toNumber (BS.unpack example)
      `shouldBe` expected

  forM_ fromNumberExamples $ \(example, expected) ->
    it ("should optimize number printing " ++ show example)
      $          fromNumber example
      `shouldBe` expected
