{-# LANGUAGE OverloadedStrings #-}

module Util.LoggingSpec
  ( spec
  ) where

import           Util.Logging                   ( say )
import qualified Data.Text                     as T
import           Control.Monad.Writer           ( Writer
                                                , runWriter
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

calc2 :: Int -> Writer [T.Text] Int
calc2 n = do
  say "start calc2"
  let n' = n - 1
  say "end calc2"
  return n'

calc :: Int -> Writer [T.Text] Int
calc n = do
  say "start calc"
  n' <- calc2 (n + 1)
  say "end calc"
  return n'

spec :: Spec
spec =
  describe "logging"
    $          it "should give right messages"
    $          runWriter (calc 1)
    `shouldBe` (1, ["start calc", "start calc2", "end calc2", "end calc"])
