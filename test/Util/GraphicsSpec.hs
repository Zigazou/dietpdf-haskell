module Util.GraphicsSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Test.Hspec (Spec, describe, it, shouldBe)

import Util.Graphics (areAligned)

areAlignedExamples :: [((Double, Double), (Double, Double), (Double, Double), Double, Bool)]
areAlignedExamples =
    [ ((0, 0)  , (1, 1)  , (2, 2)  , 0  , True)
    , ((0, 0)  , (1, 1)  , (2, 3)  , 0  , False)
    , ((0, 0)  , (1, 0)  , (1, 1)  , 0  , False)
    , ((0, 0)  , (1, 2)  , (2, 4)  , 0  , True)
    , ((0, 0)  , (-1, -1), (1, 1)  , 0  , False)
    , ((0, 0)  , (1, 1)  , (2.1, 2), 0.1, True)
    , ((30, 40), (35, 35), (40, 30), 0  , True)
    , ((30, 40), (35, 35), (40, 40), 0  , False)
    ]

spec :: Spec
spec = do
  describe "areAligned" $ forM_ areAlignedExamples $ \(p1, p2, p3, d, expected) ->
    it
        (  "should tell if the points"
        <> show p1
        <> ", "
        <> show p2
        <> ", "
        <> show p3
        <> " are aligned"
        )
      $          areAligned p1 p2 p3 d
      `shouldBe` expected
