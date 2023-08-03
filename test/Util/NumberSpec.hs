module Util.NumberSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Data.ByteString               as BS
import           Util.Number                    ( toNumber
                                                , fromNumber
                                                , bytesNeededToEncode
                                                , encodeIntToBytes
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
  , (0.00028, "0")
  , (0.0028 , ".003")
  ]

bytesNeededToEncodeExamples :: [(Int, Int)]
bytesNeededToEncodeExamples =
  [ (0         , 1)
  , (1         , 1)
  , (127       , 1)
  , (128       , 1)
  , (129       , 1)
  , (255       , 1)
  , (256       , 2)
  , (65535     , 2)
  , (65536     , 3)
  , (128000    , 3)
  , (16777215  , 3)
  , (16777216  , 4)
  , (4294967295, 4)
  , (4294967296, 5)
  ]

encodeIntToBytesExamples :: [(Int, Int, BS.ByteString)]
encodeIntToBytesExamples =
  [ (0         , 1, "\x00")
  , (1         , 1, "\x01")
  , (255       , 1, "\xff")
  , (256       , 1, "\x00")
  , (256       , 2, "\x01\x00")
  , (300       , 2, "\x01\x2c")
  , (65535     , 2, "\xff\xff")
  , (65536     , 3, "\x01\x00\x00")
  , (16777215  , 3, "\xff\xff\xff")
  , (16777216  , 4, "\x01\x00\x00\x00")
  , (162874901 , 4, "\x09\xb5\x46\x15")
  , (4294967295, 4, "\xff\xff\xff\xff")
  , (4294967296, 5, "\x01\x00\x00\x00\x00")
  , (65536     , 2, "\x00\x00")
  , (16777215  , 2, "\xff\xff")
  , (16777216  , 2, "\x00\x00")
  , (162874901 , 2, "\x46\x15")
  , (4294967295, 2, "\xff\xff")
  , (4294967296, 2, "\x00\x00")
  ]

spec :: Spec
spec = do
  describe "toNumber" $ forM_ toNumberExamples $ \(example, expected) ->
    it ("should decode bytestring " ++ show example)
      $          toNumber (BS.unpack example)
      `shouldBe` expected

  describe "fromNumber" $ forM_ fromNumberExamples $ \(example, expected) ->
    it ("should optimize number printing " ++ show example)
      $          fromNumber example
      `shouldBe` expected

  describe "bytesNeededToEncode"
    $ forM_ bytesNeededToEncodeExamples
    $ \(example, expected) ->
        it ("should calculate bytes needed to encode " ++ show example)
          $          bytesNeededToEncode example
          `shouldBe` expected

  describe "encodeIntToBytes"
    $ forM_ encodeIntToBytesExamples
    $ \(example, count, expected) ->
        it ("should encode int to minimum bytes " ++ show example)
          $          encodeIntToBytes count example
          `shouldBe` expected
