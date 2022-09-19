{-# LANGUAGE OverloadedStrings #-}
module Util.ByteStringSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Data.ByteString               as BS
import           Util.ByteString                ( splitRaw )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

splitRawExamples :: [(BS.ByteString, Int, [BS.ByteString])]
splitRawExamples =
  [ ("ABCD", 2, ["AB", "CD"])
  , ("ABCD", 1, ["A", "B", "C", "D"])
  , ("ABCD", 3, ["ABC", "D"])
  , (""    , 3, [])
  ]

spec :: Spec
spec =
  describe "splitRaw" $ forM_ splitRawExamples $ \(example, width, expected) ->
    it
        (  "should split ByteString in strings of "
        ++ show width
        ++ " bytes "
        ++ show example
        )
      $          splitRaw width example
      `shouldBe` expected
