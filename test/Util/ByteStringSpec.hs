module Util.ByteStringSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Data.ByteString               as BS
import           Util.ByteString                ( splitRaw
                                                , separateComponents
                                                , groupComponents
                                                )
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

separateComponentsExamples :: [(BS.ByteString, Int, [BS.ByteString])]
separateComponentsExamples =
  [ ("ABCD", 2, ["AC", "BD"])
  , ("ABCD", 1, ["ABCD"])
  , ("ABCD", 3, ["AD", "B", "C"])
  , (""    , 1, [""])
  ]

groupComponentsExamples :: [([BS.ByteString], BS.ByteString)]
groupComponentsExamples =
  [ (["AC", "BD"]    , "ABCD")
  , (["ABCD"]        , "ABCD")
  , (["AD", "B", "C"], "ABCD")
  , ([""]            , "")
  ]

spec :: Spec
spec = do
  describe "splitRaw" $ forM_ splitRawExamples $ \(example, width, expected) ->
    it
        (  "should split ByteString in strings of "
        ++ show width
        ++ " bytes "
        ++ show example
        )
      $          splitRaw width example
      `shouldBe` expected

  describe "separateComponents"
    $ forM_ separateComponentsExamples
    $ \(example, components, expected) ->
        it
            (  "should separate components strings of "
            ++ show components
            ++ " components "
            ++ show example
            )
          $          separateComponents components example
          `shouldBe` expected

  describe "groupComponents"
    $ forM_ groupComponentsExamples
    $ \(example, expected) ->
        it ("should group components strings " ++ show example)
          $          groupComponents example
          `shouldBe` expected
