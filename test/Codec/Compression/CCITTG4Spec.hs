module Codec.Compression.CCITTG4Spec
  ( spec
  ) where

import Codec.Compression.CCITTG4 (decodeG4, encodeG4)
import Control.Monad (forM_)
import Data.Bits (setBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (foldl')
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck
  ( Gen, arbitrary, chooseInt, elements, forAll, frequency, vectorOf
  , withMaxSuccess, (===)
  )

-- Pack each row separately: pixels are MSB first and unused low bits are zero.
packPixels :: [Bool] -> ByteString
packPixels [] = BS.empty
packPixels pixels = BS.cons byte (packPixels rest)
  where
    (chunk, rest) = splitAt 8 pixels
    byte = foldl' (\acc (bit, black) ->
                    if black then setBit acc bit else acc)
                  0 (zip [7,6..0] chunk)

randomMask :: Gen (Int, Int, ByteString)
randomMask = do
  width <- frequency
    [ (4, chooseInt (1, 128))
    , (1, elements [1, 7, 8, 9, 63, 64, 65, 127, 128, 129])
    ]
  height <- chooseInt (0, 16)
  rows <- vectorOf height $ frequency
    [ (6, vectorOf width arbitrary)
    , (1, pure (replicate width False))
    , (1, pure (replicate width True))
    , (2, do
          start <- chooseInt (0, width)
          end <- chooseInt (start, width)
          pure (replicate start False ++ replicate (end - start) True
                ++ replicate (width - end) False))
    ]
  pure (width, height, BS.concat (map packPixels rows))

-- Exercise terminating, makeup, common makeup and repeated makeup runs.
longRunMask :: Gen (Int, Int, ByteString)
longRunMask = do
  width <- elements [63, 64, 65, 1728, 1792, 2560, 2624, 5120, 5121]
  white <- chooseInt (0, width)
  height <- chooseInt (1, 3)
  let row = packPixels (replicate white False ++ replicate (width - white) True)
  pure (width, height, BS.concat (replicate height row))

-- Hand-derived from ITU-T T.6 (11/1988), Tables 1, 2 and 3:
-- https://www.itu.int/rec/T-REC-T.6-198811-I/en
-- These are raw coding bits, without EOFB or alignment between rows.
-- Expected streams are independent of encodeG4; only the final byte is padded.
vectors :: [(String, Int, Int, ByteString, String)]
vectors =
  [ ("white row: V(0)", 8, 1, BS.pack [0x00], "1")
  , ("white rows without byte alignment", 9, 3, BS.replicate 6 0, "111")
  , ("black row: H, white(0), black(8)", 8, 1, BS.pack [0xff],
      "001 00110101 000101")
  , ("repeated black rows with a change at x=0", 8, 2, BS.pack [0xff, 0xff],
      "001 00110101 000101 1 1")
  , ("pass over a black run", 16, 2, BS.pack [0x0f, 0x00, 0x00, 0x00],
      "001 1011 011 1 0001 1")
  , ("black makeup(64) and terminating(0)", 64, 1, BS.replicate 8 0xff,
      "001 00110101 0000001111 0000110111")
  , ("black makeup(512) and terminating(0)", 512, 1, BS.replicate 64 0xff,
      "001 00110101 0000001101100 0000110111")
  , ("black makeup(576) and terminating(0)", 576, 1, BS.replicate 72 0xff,
      "001 00110101 0000001101101 0000110111")
  , ("white makeup(64) followed by black(8)", 72, 1,
      BS.replicate 8 0 <> BS.singleton 0xff,
      "001 11011 00110101 000101")
  ] ++
  [ ("vertical displacement " ++ show displacement, 16, 2,
      BS.pack [0x00, 0xff] <>
        packPixels (replicate (8 + displacement) False ++
                    replicate (8 - displacement) True),
      "001 10011 000101 " ++ code ++ " 1")
  | (displacement, code) <-
      [ (-3, "0000010"), (-2, "000010"), (-1, "010"), (0, "1")
      , (1, "011"), (2, "000011"), (3, "0000011")
      ]
  ]

spec :: Spec
spec = describe "CCITTG4" $ do
  it "round-trips random masks with zero row padding" $
    withMaxSuccess 500 $ forAll randomMask $ \(width, height, bitmap) ->
      (decodeG4 width height =<< encodeG4 width height bitmap) === Right bitmap

  it "round-trips long runs and repeated reference rows" $
    forAll longRunMask $ \(width, height, bitmap) ->
      (decodeG4 width height =<< encodeG4 width height bitmap) === Right bitmap

  it "round-trips an empty image" $ do
    encodeG4 9 0 BS.empty `shouldBe` Right BS.empty
    decodeG4 9 0 BS.empty `shouldBe` Right BS.empty

  forM_ vectors $ \(name, width, height, bitmap, codes) ->
    describe name $ do
      let encoded = packPixels (map (== '1') (filter (/= ' ') codes))
      it "decodes the T.6 vector" $
        decodeG4 width height encoded `shouldBe` Right bitmap
      it "encodes to the T.6 vector" $
        encodeG4 width height bitmap `shouldBe` Right encoded
