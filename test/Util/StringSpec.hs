module Util.StringSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.ByteString qualified as BS

import Test.Hspec (Spec, describe, it, shouldBe)

import Util.String (hexStringToString)

hexStringExamples :: [(BS.ByteString, BS.ByteString)]
hexStringExamples =
  [ ("48656C6C6F2C20576F726C6421", "Hello, World!")
  , (""                          , "")
  , ("1"                         , "\x10")
  , ("10"                        , "\x10")
  ]

spec :: Spec
spec =
  describe "hexStringToString"
    $ forM_ hexStringExamples
    $ \(example, expected) ->
        it ("should convert PDFHexString to PDFString " ++ show example)
          $          hexStringToString example
          `shouldBe` expected
