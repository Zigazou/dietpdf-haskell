{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}
module Util.OrderedSetSpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Data.Set.Ordered              as OS
import           Util.OrderedSet                ( fromMostRecents )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

data FooBar = FooBar
  { major :: Int
  , minor :: Int
  }
  deriving stock (Eq, Show)

instance Ord FooBar where
  compare (FooBar major1 _) (FooBar major2 _) = compare major1 major2

fromMostRecentsExamples :: [([FooBar], OS.OSet FooBar)]
fromMostRecentsExamples =
  [ ([FooBar 1 0, FooBar 1 1]            , OS.fromList [FooBar 1 1])
  , ([FooBar 1 0, FooBar 2 0, FooBar 1 1], OS.fromList [FooBar 2 0, FooBar 1 1])
  ]

spec :: Spec
spec = describe "fromListRecent" $ do
  forM_ fromMostRecentsExamples $ \(example, expected) ->
    it ("should ignore older items " ++ show example)
      $          fromMostRecents example
      `shouldBe` expected
