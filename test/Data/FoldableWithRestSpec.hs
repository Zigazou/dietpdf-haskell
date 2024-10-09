module Data.FoldableWithRestSpec
  ( spec
  )
  where


import Data.FoldableWithRest (foldMWithRest, foldWithRest)
import Data.Sequence (Seq, fromList)

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "FoldableWithRest for lists" $ do
    it "foldWithRest should accumulate correctly for lists" $ do
      let
        func :: Int -> Int -> [Int] -> Int
        func acc x rest = acc + x + sum rest
        result = foldWithRest func 0 [1, 2, 3, 4]
      result `shouldBe` 30  -- 0 + (1 + (2+3+4)) + (2 + (3+4)) + (3 + 4) + 4

    it "foldMWithRest should accumulate correctly for lists in a monadic context" $ do
      let
        func :: Int -> Int -> [Int] -> Maybe Int
        func acc x rest = Just (acc + x + sum rest)
        result = foldMWithRest func 0 [1, 2, 3, 4]
      result `shouldBe` Just 30

  describe "FoldableWithRest for sequences" $ do
    it "foldWithRest should accumulate correctly for sequences" $ do
      let
        func :: Int -> Int -> Seq Int -> Int
        func acc x rest = acc + x + sum rest
        result = foldWithRest func 0 (fromList [1, 2, 3, 4] :: Seq Int)
      result `shouldBe` 30

    it "foldMWithRest should accumulate correctly for sequences in a monadic context" $ do
      let
        func :: Int -> Int -> Seq Int -> Maybe Int
        func acc x rest = Just (acc + x + sum rest)
        result = foldMWithRest func 0 (fromList [1, 2, 3, 4] :: Seq Int)
      result `shouldBe` Just 30
