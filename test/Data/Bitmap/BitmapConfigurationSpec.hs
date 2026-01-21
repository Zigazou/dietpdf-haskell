module Data.Bitmap.BitmapConfigurationSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.Bitmap.BitmapConfiguration
  ( BitmapConfiguration (BitmapConfiguration)
  , bitmapRawWidth
  , findBitmapConfigurations
  )
import Data.Bitmap.BitsPerComponent
  (BitsPerComponent (BC16Bits, BC2Bits, BC4Bits, BC8Bits))

import Test.Hspec (Spec, describe, it, shouldBe)

bitmapRawWidthExamples :: [(BitmapConfiguration, Int)]
bitmapRawWidthExamples =
  [ (BitmapConfiguration 1 1 BC8Bits, 1)
  , (BitmapConfiguration 2 1 BC8Bits, 2)
  , (BitmapConfiguration 3 1 BC8Bits, 3)
  , (BitmapConfiguration 1 3 BC8Bits, 3)
  , (BitmapConfiguration 2 3 BC8Bits, 6)
  , (BitmapConfiguration 3 3 BC8Bits, 9)

    -- 4 bits per component
  , (BitmapConfiguration 1 1 BC4Bits, 1)
  , (BitmapConfiguration 2 1 BC4Bits, 1)
  , (BitmapConfiguration 3 1 BC4Bits, 2)
  , (BitmapConfiguration 1 3 BC4Bits, 2)
  , (BitmapConfiguration 2 3 BC4Bits, 3)
  , (BitmapConfiguration 3 3 BC4Bits, 5)

    -- 2 bits per component
  , (BitmapConfiguration 1 1 BC2Bits, 1)
  , (BitmapConfiguration 2 1 BC2Bits, 1)
  , (BitmapConfiguration 3 1 BC2Bits, 1)
  , (BitmapConfiguration 4 1 BC2Bits, 1)
  , (BitmapConfiguration 5 1 BC2Bits, 2)
  , (BitmapConfiguration 1 3 BC2Bits, 1)
  , (BitmapConfiguration 2 3 BC2Bits, 2)
  , (BitmapConfiguration 3 3 BC2Bits, 3)
  , (BitmapConfiguration 4 3 BC2Bits, 3)
  , (BitmapConfiguration 5 3 BC2Bits, 4)
  , (BitmapConfiguration 14 3 BC2Bits, 11)

    -- 16 bits per component
  , (BitmapConfiguration 1 1 BC16Bits, 2)
  , (BitmapConfiguration 2 1 BC16Bits, 4)
  , (BitmapConfiguration 3 1 BC16Bits, 6)
  , (BitmapConfiguration 1 3 BC16Bits, 6)
  , (BitmapConfiguration 2 3 BC16Bits, 12)
  , (BitmapConfiguration 3 3 BC16Bits, 18)
  ]

findBitmapConfigurationsExamples :: [(Int, [BitmapConfiguration])]
findBitmapConfigurationsExamples =
  [ ( 0, [] )
  , ( 1
    , [ BitmapConfiguration 4 1 BC2Bits
      , BitmapConfiguration 2 2 BC2Bits
      , BitmapConfiguration 1 4 BC2Bits
      , BitmapConfiguration 2 1 BC4Bits
      , BitmapConfiguration 1 2 BC4Bits
      , BitmapConfiguration 1 1 BC8Bits
      ]
    )
  , ( 2,
      [ BitmapConfiguration 8 1 BC2Bits
      , BitmapConfiguration 4 2 BC2Bits
      , BitmapConfiguration 2 4 BC2Bits
      , BitmapConfiguration 4 1 BC4Bits
      , BitmapConfiguration 2 2 BC4Bits
      , BitmapConfiguration 1 4 BC4Bits
      , BitmapConfiguration 2 1 BC8Bits
      , BitmapConfiguration 1 2 BC8Bits
      , BitmapConfiguration 1 1 BC16Bits
      ]
    )
  , ( 3
    , [ BitmapConfiguration 12 1 BC2Bits
      , BitmapConfiguration 6 2 BC2Bits
      , BitmapConfiguration 4 3 BC2Bits
      , BitmapConfiguration 3 4 BC2Bits
      , BitmapConfiguration 6 1 BC4Bits
      , BitmapConfiguration 3 2 BC4Bits
      , BitmapConfiguration 2 3 BC4Bits
      , BitmapConfiguration 3 1 BC8Bits
      , BitmapConfiguration 1 3 BC8Bits
      ]
    )
  , ( 4
    , [ BitmapConfiguration 16 1 BC2Bits
      , BitmapConfiguration 8 2 BC2Bits
      , BitmapConfiguration 4 4 BC2Bits
      , BitmapConfiguration 8 1 BC4Bits
      , BitmapConfiguration 4 2 BC4Bits
      , BitmapConfiguration 2 4 BC4Bits
      , BitmapConfiguration 4 1 BC8Bits
      , BitmapConfiguration 2 2 BC8Bits
      , BitmapConfiguration 1 4 BC8Bits
      , BitmapConfiguration 2 1 BC16Bits
      , BitmapConfiguration 1 2 BC16Bits
      ]
    )
  , ( 5
    , [ BitmapConfiguration 20 1 BC2Bits
      , BitmapConfiguration 10 2 BC2Bits
      , BitmapConfiguration 5 4 BC2Bits
      , BitmapConfiguration 10 1 BC4Bits
      , BitmapConfiguration 5 2 BC4Bits
      , BitmapConfiguration 5 1 BC8Bits
      ]
    )
  , ( 6
    , [ BitmapConfiguration 24 1 BC2Bits
      , BitmapConfiguration 12 2 BC2Bits
      , BitmapConfiguration 8 3 BC2Bits
      , BitmapConfiguration 6 4 BC2Bits
      , BitmapConfiguration 12 1 BC4Bits
      , BitmapConfiguration 6 2 BC4Bits
      , BitmapConfiguration 4 3 BC4Bits
      , BitmapConfiguration 3 4 BC4Bits
      , BitmapConfiguration 6 1 BC8Bits
      , BitmapConfiguration 3 2 BC8Bits
      , BitmapConfiguration 2 3 BC8Bits
      , BitmapConfiguration 3 1 BC16Bits
      , BitmapConfiguration 1 3 BC16Bits
      ]
    )
  , ( 7
    , [ BitmapConfiguration 28 1 BC2Bits
      , BitmapConfiguration 14 2 BC2Bits
      , BitmapConfiguration 7 4 BC2Bits
      , BitmapConfiguration 14 1 BC4Bits
      , BitmapConfiguration 7 2 BC4Bits
      , BitmapConfiguration 7 1 BC8Bits
      ]
    )
  ]


spec :: Spec
spec = do
  describe "bitmapRawWidth"
    $ forM_ bitmapRawWidthExamples
    $ \(bitmapConfig, expected) ->
        it ("should calculate raw width for " ++ show bitmapConfig)
          $          bitmapRawWidth bitmapConfig
          `shouldBe` expected

  describe "findBitmapConfigurations"
    $ forM_ findBitmapConfigurationsExamples
    $ \(rawWidth, expected) ->
        it ("should find bitmap configurations for raw width " ++ show rawWidth)
          $          findBitmapConfigurations rawWidth
          `shouldBe` expected
