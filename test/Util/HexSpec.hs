module Util.HexSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.ByteString qualified as BS

import Test.Hspec (Spec, describe, it, shouldBe)

import Util.Hex (fromHexDigits, toHexDigits)

fromHexDigitsExamples :: [(BS.ByteString, BS.ByteString)]
fromHexDigitsExamples =
  [ (""           , "")
  , ("     "      , "")
  , ("AB"         , "\xAB")
  , ("  A   B "   , "\xAB")
  , ("ab"         , "\xAB")
  , ("  a   b "   , "\xAB")
  , ("13"         , "\x13")
  , ("  1   3 "   , "\x13")
  , ("A"          , "\xA0")
  , ("  A   "     , "\xA0")
  , ("a"          , "\xA0")
  , ("  a   "     , "\xA0")
  , ("1"          , "\x10")
  , ("  1  "      , "\x10")
  , ("abcdefgh12" , "\xAB\xCD\xEF\x12")
  , ("abcdefgh1yz", "\xAB\xCD\xEF\x10")
  ]

toHexDigitsExamples :: [(BS.ByteString, BS.ByteString)]
toHexDigitsExamples =
  [("\x12\x34\x56\x78\x9a\xbc\xde\xf0", "123456789abcdef0")]

spec :: Spec
spec = do
  describe "fromHexDigits" $ do
    forM_ fromHexDigitsExamples $ \(example, expected) ->
      it ("should decode hex digits " ++ show example)
        $          fromHexDigits example
        `shouldBe` expected

  describe "toHexDigits" $ do
    forM_ toHexDigitsExamples $ \(example, expected) ->
      it ("should encode hex digits " ++ show example)
        $          toHexDigits example
        `shouldBe` expected
