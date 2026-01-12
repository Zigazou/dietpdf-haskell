module Font.TrueType.Parser.GlyfSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.Binary.Parser (parseOnly)
import Data.ByteString (ByteString)

import Font.TrueType.FontTable.GlyphTable
  ( Glyph (EmptyGlyph, SimpleGlyph, CompositeGlyph)
  , GlyphHeader (GlyphHeader, ghNumberOfContours, ghXMax, ghXMin, ghYMax, ghYMin)
  , SimpleGlyphData (SimpleGlyphData, sgEndPtsOfContours, sgFlags, sgInstructionLength, sgInstructions, sgXCoordinates, sgYCoordinates)
  )
import Font.TrueType.Parser.Glyf (singleGlyphP, glyphHeaderP)

import Test.Hspec (Spec, describe, it, shouldBe)

glyphHeaderPExamples :: [(ByteString, GlyphHeader)]
glyphHeaderPExamples =
  [ ( "\x00\x05\x00\x64\x00\xC8\x01\x90\x02\x58"
    , GlyphHeader
        { ghNumberOfContours = 5
        , ghXMin = 100
        , ghYMin = 200
        , ghXMax = 400
        , ghYMax = 600
        }
    )
  , ( "\xFF\xFF\x00\x00\x00\x00\x01\x00\x01\x00"
    , GlyphHeader
        { ghNumberOfContours = -1  -- composite glyph
        , ghXMin = 0
        , ghYMin = 0
        , ghXMax = 256
        , ghYMax = 256
        }
    )
  ]

glyphPExamples :: [(ByteString, Glyph)]
glyphPExamples =
  [ ( ""  -- Empty glyph
    , EmptyGlyph
    )
  , ( "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"  -- Glyph with 0 contours
    , EmptyGlyph
    )
  ]

spec :: Spec
spec = do
  describe "glyphHeaderP" $ do
    forM_ glyphHeaderPExamples $ \(source, expected) ->
      it ("decodes glyph header " ++ show source) $ do
        parseOnly glyphHeaderP source `shouldBe` Right expected

  describe "singleGlyphP" $ do
    forM_ glyphPExamples $ \(source, expected) ->
      it ("decodes glyph " ++ show source) $ do
        parseOnly singleGlyphP source `shouldBe` Right expected
