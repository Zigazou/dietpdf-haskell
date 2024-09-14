module Data.BitsArraySpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.BitsArray
    ( BitsArray (BitsArray)
    , appendBits
    , newBitsArray
    , toByteString
    , word64ToWord8List
    )
import Data.ByteString (ByteString)
import Data.Vector.Storable qualified as VS
import Data.Word (Word64, Word8)

import Test.Hspec (Spec, describe, it, shouldBe)

word64ToWord8ListExamples :: [(String, Int, Word64, [Word8])]
word64ToWord8ListExamples =
   [ ( "a"
     , 8
     , 0x1122_3344_5566_7788
     , [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88]
     )
   , ( "b"
     , 4
     , 0x1122_3344_5566_7788
     , [0x55, 0x66, 0x77, 0x88]
     )
   , ( "c"
     , 2
     , 0x1122_3344_5566_7788
     , [0x77, 0x88]
     )
   , ( "d"
     , 1
     , 0x1122_3344_5566_7788
     , [0x88]
     )
   , ( "e"
     , 0
     , 0x1122_3344_5566_7788
     , []
     )
   ]

appendBitsExamples :: [(String, BitsArray, Int, Int, BitsArray)]
appendBitsExamples =
    [ ( "a"
      , newBitsArray 1
      , 0
      , 0
      , newBitsArray 1
      )
    , ( "b"
      , newBitsArray 1
      , 2
      , 0x03
      , BitsArray 0 1 2 (VS.fromList [0xC0])
      )
    , ( "c"
      , newBitsArray 3
      , 12
      , 0x0103
      , BitsArray 0 1 12 (VS.fromList [0x10, 0x30, 0x00])
      )
    , ( "d"
      , newBitsArray 8
      , 63
      , 0b00100000_11111111_11111111_11111111_11111111_11111111_11111111_11111111
      , BitsArray 0 1 63 (VS.fromList [0x41, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFE])
      )
    , ( "e"
      , BitsArray 0 1 12 (VS.fromList [0x10, 0x30, 0x00])
      , 12
      , 0x0103
      , BitsArray 0 1 24 (VS.fromList [0x10, 0x31, 0x03])
      )
    , ( "f"
      , BitsArray 0 1 11 (VS.fromList [0x10, 0x20, 0x00])
      , 12
      , 0x0103
      , BitsArray 0 1 23 (VS.fromList [0x10, 0x22, 0x06])
      )
    , ( "g"
      , BitsArray 0 1 9 (VS.fromList [0x80, 0x00, 0x00])
      , 9
      , 0x157
      , BitsArray 0 1 18 (VS.fromList [0x80, 0x55, 0xC0])
      )
    , ( "h"
      , BitsArray 0 1 18 (VS.fromList [0x80, 0x55, 0xC0, 0x00])
      , 9
      , 0x157
      , BitsArray 0 1 27 (VS.fromList [0x80, 0x55, 0xEA, 0xE0])
      )
    , ( "i"
      , BitsArray 0 1 36 (VS.fromList [0x80, 0x19, 0x0c, 0x86, 0x10, 0x00])
      , 9
      , 0x101
      , BitsArray 0 1 45 (VS.fromList [0x80, 0x19, 0x0c, 0x86, 0x18, 0x08])
      )
    ]

toByteStringExamples :: [(String, BitsArray, ByteString)]
toByteStringExamples =
    [ ( "a"
      , BitsArray 0 1 2 (VS.fromList [0xC0])
      , "\xC0"
      )
    , ( "b"
      , BitsArray 0 1 2 (VS.fromList [0xC0, 0x00, 0x00, 0x00, 0x00])
      , "\xC0"
      )
    , ( "c"
      , BitsArray 0 1 63 (VS.fromList [0x41, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFE])
      , "\x41\xFF\xFF\xFF\xFF\xFF\xFF\xFE"
      )
    , ( "d"
      , BitsArray 0 1 45 (VS.fromList [0x80, 0x19, 0x0c, 0x86, 0x18, 0x08])
      , "\x80\x19\x0c\x86\x18\x08"
      )
    ]

spec :: Spec
spec = do
  describe "appendBits"
    $ forM_ appendBitsExamples
    $ \(label, source, bitWidth, bits, expected) ->
        it ("should append bits for example " ++ label)
          $          appendBits bitWidth bits source
          `shouldBe` expected

  describe "toByteString"
    $ forM_ toByteStringExamples
    $ \(label, source, expected) ->
        it ("should convert bits array to byte string for example " ++ label)
          $          toByteString source
          `shouldBe` expected


  describe "word64ToWord8List"
    $ forM_ word64ToWord8ListExamples
    $ \(label, len, value, expected) ->
        it ("should expand word64 to word8 list for example " ++ label)
          $          word64ToWord8List len value
          `shouldBe` expected
