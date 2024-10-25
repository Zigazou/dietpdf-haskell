module Codec.Compression.LZWSpec
  ( spec
  ) where


import Codec.Compression.LZW (compress, decompress)

import Control.Monad (forM_)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

errorExamples :: [ByteString]
errorExamples =
  [ "a"
  , "aa"
  ,  "\x78\x5e\xf3\x48\xcd\xc9\xc9\x57\x28\xcf\x2f\xca\x49\x51\xe4\x02\x00\x21\x71\x04\x69"
  ]

examples :: [(ByteString, ByteString)]
examples =
  [ ( "\x80\x0B\x60\x50\x22\x0C\x0C\x85\x01"
    , "\x2D\x2D\x2D\x2D\x2D\x41\x2D\x2D\x2D\x42"
    )
  ]

isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft (Right _) = False

spec :: Spec
spec = do
  describe "decompress" $ do
    forM_ errorExamples $ \example ->
      it ("should generate an error while decoding " ++ show example)
        $          decompress example
        `shouldSatisfy` isLeft

    forM_ examples $ \(example, expected) ->
      it ("decodes example " ++ show example)
        $          decompress example
        `shouldBe` Right expected

    it "decodes test file" $ do
      compressed <- BS.readFile "test/Codec/Compression/lzw_test.compressed"
      uncompressed <- BS.readFile "test/Codec/Compression/lzw_test.uncompressed"
      decompress compressed `shouldBe` Right uncompressed

  describe "compress" $ do
    forM_ examples $ \(expected, example) ->
      it ("encodes example " ++ show example)
        $          compress example
        `shouldBe` Right expected

    it "encodes/decodes test file 2" $ do
      uncompressed <- BS.readFile "test/Codec/Compression/lzw_test2.uncompressed"
      (compress uncompressed >>= decompress) `shouldBe` Right uncompressed

    it "encodes/decodes test file 3" $ do
      uncompressed <- BS.readFile "test/Codec/Compression/lzw_test3.uncompressed"
      (compress uncompressed >>= decompress) `shouldBe` Right uncompressed

    it "encodes/decodes test file 4" $ do
      uncompressed <- BS.readFile "test/Codec/Compression/lzw_test4.uncompressed"
      (compress uncompressed >>= decompress) `shouldBe` Right uncompressed

    it "encodes/decodes test file 5" $ do
      uncompressed <- BS.readFile "test/Codec/Compression/lzw_test5.uncompressed"
      (compress uncompressed >>= decompress) `shouldBe` Right uncompressed

