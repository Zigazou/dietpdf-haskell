module Util.TransformSpec
  ( spec
  ) where

import Control.Exception (evaluate)
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe)

import Util.Transform (untilNoImprovement)

spec :: Spec
spec = describe "untilNoImprovement" $ do
  it "returns the input without transforming for a non-positive allowance" $ do
    untilNoImprovement 0 id (\_ -> error "Unexpected transformation") (10 :: Int) `shouldBe` 10
    untilNoImprovement (-1) id (\_ -> error "Unexpected transformation") (10 :: Int) `shouldBe` 10

  it "keeps the original when every transformation makes it larger" $
    untilNoImprovement 3 id (+ 1) (10 :: Int) `shouldBe` 10

  it "crosses regressions and resets the allowance at each new minimum" $
    untilNoImprovement 2 head tail [10, 12, 8, 9, 6, 7, 8 :: Int]
      `shouldBe` [6, 7, 8]

  it "stops exactly when the allowance is exhausted" $
    untilNoImprovement 2 head tail [10, 11, 12, 1 :: Int]
      `shouldBe` [10, 11, 12, 1]

  it "retains the earlier result on ties" $
    untilNoImprovement 2 head tail [10, 10, 10, 1 :: Int]
      `shouldBe` [10, 10, 10, 1]

  it "terminates on cycles and returns the best result" $
    untilNoImprovement 3 id (\n -> if n == 2 then 1 else 2) (2 :: Int)
      `shouldBe` 1

  it "stops immediately at a fixed point even with a large allowance" $ do
    result <- timeout 1000000 $
      evaluate (untilNoImprovement maxBound id id (10 :: Int))
    result `shouldBe` Just 10

  it "returns the earlier best result when a regression reaches a fixed point" $ do
    result <- timeout 1000000 $
      evaluate (untilNoImprovement maxBound id (min 12 . (+ 1)) (10 :: Int))
    result `shouldBe` Just 10
