module Codec.Compression.FlateSpec
  ( spec
  ) where

import Codec.Compression.Flate (compress, decompress)

import Control.Monad (forM, forM_)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.UnifiedError (UnifiedError (FlateDecodeError))
import Data.Word (Word8)

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, arbitrary, forAll)

errorExamples :: [(ByteString, Fallible ByteString)]
errorExamples =
  [ ( "a"
    , Left $ FlateDecodeError
      "Codec.Compression.Zlib: premature end of compressed data stream"
    )
  , ( "\x78\x5e\xf3\x48\xcd\xc9\xc9\x57\x28\xcf\x2f\xca\x49\x51\xe4\x02\x00\x21\x71\x04\x69"
    , Left
      $ FlateDecodeError
          "Codec.Compression.Zlib: compressed data stream format error (incorrect data check)"
    )
  ]

randomString :: Gen ByteString
randomString = do
  items         <- arbitrary :: Gen [Word8]
  expandedItems <- forM items $ \item -> do
    replicateCount <- arbitrary
    return $ replicate replicateCount item
  return $ BS.pack (concat expandedItems)

spec :: Spec
spec = describe "runLength" $ do
  forM_ errorExamples $ \(example, expected) ->
    it ("should generate an error while decoding " ++ show example)
      $          decompress example
      `shouldBe` expected

  it "encode then decode should give the same value"
    $ forAll randomString
    $ \x -> (compress x >>= decompress) `shouldBe` Right x
