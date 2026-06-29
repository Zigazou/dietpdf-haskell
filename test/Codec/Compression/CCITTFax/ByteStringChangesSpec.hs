module Codec.Compression.CCITTFax.ByteStringChangesSpec (spec) where

import Codec.Compression.CCITTFax.ByteStringChanges
  (ByteChange (ByteChange), byteStringChanges, nextDifferent)

import Control.Monad (forM_)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Vector qualified as V

import Test.Hspec (Spec, describe, it, shouldBe)

byteStringChangesExamples :: [(ByteString, ByteChange)]
byteStringChangesExamples =
  [ ( BS.pack [0b00000000]
    , ByteChange False (-1) (V.fromList [])
    )
  , ( BS.pack [0b11111111]
    , ByteChange False (-1) (V.fromList [0])
    )
  , ( BS.pack [0b10101010]
    , ByteChange False (-1) (V.fromList [0,1,2,3,4,5,6,7])
    )
  , ( BS.pack [0b10101010, 0b10101010]
    , ByteChange False (-1) (V.fromList [0,1,2,3,4,5,6,7, 8,9,10,11,12,13,14,15])
    )
  , ( BS.pack [0b00001111, 0b11110000]
    , ByteChange False (-1) (V.fromList [4, 12])
    )
  , ( BS.pack [0b11100000, 0b01111000, 0b00011111]
    , ByteChange False (-1) (V.fromList [0, 3, 9, 13, 19])
    )
  , ( BS.pack []
    , ByteChange False (-1) (V.fromList [])
    )
  ]

nextDifferentExamples :: [(ByteChange, ByteChange, Maybe ByteChange)]
nextDifferentExamples =
  [ ( ByteChange True (-1) (V.fromList [2, 7, 10])
    , ByteChange False (-1) (V.fromList [1, 3, 8, 12])
    , Just (ByteChange False 1 (V.fromList [1, 3, 8, 12]))
    )
  , ( ByteChange False (-1) (V.fromList [2, 7, 10])
    , ByteChange False (-1) (V.fromList [1, 3, 8, 12])
    , Just (ByteChange True 0 (V.fromList [1, 3, 8, 12]))
    )
  , ( ByteChange False 0 (V.fromList [2, 7, 10])
    , ByteChange True 0 (V.fromList [1, 3, 8, 12])
    , Just (ByteChange True 2 (V.fromList [1, 3, 8, 12]))
    )
  , ( ByteChange False (-1) (V.fromList [])
    , ByteChange False (-1) (V.fromList [1, 3, 8, 12])
    , Just (ByteChange True 0 (V.fromList [1, 3, 8, 12]))
    )
  ]

spec :: Spec
spec = do
  describe "byteStringChanges" $ do
    forM_ byteStringChangesExamples $ \(example, expected) -> do
      it ("give changes for " ++ show example) $ do
        byteStringChanges False example `shouldBe` expected

  describe "nextDifferent" $ do
    forM_ nextDifferentExamples $ \(bc1, bc2, expected) -> do
      it ("gives next different between " ++ show bc1 ++ " and " ++ show bc2) $ do
        nextDifferent bc1 bc2 `shouldBe` expected
