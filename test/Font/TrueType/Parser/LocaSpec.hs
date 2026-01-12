module Font.TrueType.Parser.LocaSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.Binary.Parser (parseOnly)
import Data.ByteString (ByteString)

import Font.TrueType.FontTable.LocationTable
  ( LocationTable (LocationTable)
  )
import Font.TrueType.Parser.Loca (LocaFormat (LongFormat, ShortFormat), locaP)

import Test.Hspec (Spec, describe, it, shouldBe)

-- Test examples for short format loca table
shortFormatExamples :: [(ByteString, Int, LocationTable)]
shortFormatExamples =
  [ -- Simple case: 3 glyphs (so 4 entries)
    -- Offsets: 0, 10, 20, 30 (in bytes)
    -- Short format: divide by 2 -> 0, 5, 10, 15
    ( "\x00\x00\x00\x05\x00\x0A\x00\x0F"
    , 3  -- numGlyphs
    , LocationTable [0, 10, 20, 30]
    )
  , -- Edge case: empty glyphs (same consecutive offsets)
    -- Offsets: 0, 0, 100, 100
    -- Short format: 0, 0, 50, 50
    ( "\x00\x00\x00\x00\x00\x32\x00\x32"
    , 3
    , LocationTable [0, 0, 100, 100]
    )
  , -- Single glyph
    -- Offsets: 0, 24
    -- Short format: 0, 12
    ( "\x00\x00\x00\x0C"
    , 1
    , LocationTable [0, 24]
    )
  ]

-- Test examples for long format loca table
longFormatExamples :: [(ByteString, Int, LocationTable)]
longFormatExamples =
  [ -- Simple case: 3 glyphs (so 4 entries)
    -- Offsets: 0, 100, 200, 300
    ( "\x00\x00\x00\x00\x00\x00\x00\x64\x00\x00\x00\xC8\x00\x00\x01\x2C"
    , 3  -- numGlyphs
    , LocationTable [0, 100, 200, 300]
    )
  , -- Larger offsets that require 32 bits
    -- Offsets: 0, 65536, 131072
    ( "\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00"
    , 2
    , LocationTable [0, 65536, 131072]
    )
  , -- Single glyph with large offset
    -- Offsets: 0, 1000000
    ( "\x00\x00\x00\x00\x00\x0F\x42\x40"
    , 1
    , LocationTable [0, 1000000]
    )
  ]

spec :: Spec
spec = do
  describe "locaP with ShortFormat" $ do
    forM_ shortFormatExamples $ \(source, numGlyphs, expected) ->
      it ("decodes short format with " ++ show numGlyphs ++ " glyphs") $ do
        parseOnly (locaP ShortFormat numGlyphs) source `shouldBe` Right expected

  describe "locaP with LongFormat" $ do
    forM_ longFormatExamples $ \(source, numGlyphs, expected) ->
      it ("decodes long format with " ++ show numGlyphs ++ " glyphs") $ do
        parseOnly (locaP LongFormat numGlyphs) source `shouldBe` Right expected
