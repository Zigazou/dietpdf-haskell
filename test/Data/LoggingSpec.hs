{-# LANGUAGE OverloadedStrings #-}

module Data.LoggingSpec
  ( spec
  ) where

import Control.Monad.Writer (Writer, runWriter)

import Data.Context (Context (NoContext))
import Data.Logging (say)
import Data.Text qualified as T

import Test.Hspec (Spec, describe, it, shouldBe)

calc2 :: Int -> Writer [T.Text] Int
calc2 n = do
  say NoContext "start calc2"
  let n' = n - 1
  say NoContext "end calc2"
  return n'

calc :: Int -> Writer [T.Text] Int
calc n = do
  say NoContext "start calc"
  n' <- calc2 (n + 1)
  say NoContext "end calc"
  return n'

resultsTrim :: (Int, [T.Text]) -> (Int, [T.Text])
resultsTrim (result, logs) = (result, fmap T.strip logs)

spec :: Spec
spec =
  describe "logging"
    $          it "should give right messages"
    $          resultsTrim (runWriter (calc 1))
    `shouldBe` (1, ["start calc", "start calc2", "end calc2", "end calc"])
