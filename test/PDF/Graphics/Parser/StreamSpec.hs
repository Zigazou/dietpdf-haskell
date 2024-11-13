module PDF.Graphics.Parser.StreamSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.Array (mkArray)
import Data.ByteString (ByteString)
import Data.PDF.GFXObject
  ( GFXObject (GFXComment, GFXInlineImage, GFXName, GFXNumber, GFXOperator, GFXString, GFXString)
  , GSOperator (GSBeginText, GSEndText, GSIntersectClippingPathEOR, GSSetCharacterSpacing, GSSetGlyphWidth, GSSetNonStrokeRGBColorspace, GSSetTextFont, GSSetTextMatrix, GSSetWordSpacing, GSShowText, GSSaveGS, GSRectangle, GSSetLineWidth)
  , mkGFXArray
  )
import Data.PDF.GFXObjects (GFXObjects)

import PDF.Graphics.Parser.Stream (gfxParse)

import Test.Hspec (Spec, describe, it, shouldBe)

import Util.Dictionary (mkDictionary)

streamExamples :: [(Int, ByteString, GFXObjects)]
streamExamples =
  [ ( 0
    , "BT /F1 1 Tf 64 0 0 64 7.1771 2.4414 Tm 0 Tc \
      \W* 0 Tw % Comment\n1.0 0.0 0.0 rg (\\001) Tj ET \
      \BI /W 17 /H 17 /CS /RGB /BPC 8 /F [ /A85 /LZW ] ID ABCD EI"
    , mkArray
      [ GFXOperator GSBeginText
      , GFXName "F1"
      , GFXNumber 1.0
      , GFXOperator GSSetTextFont
      , GFXNumber 64.0
      , GFXNumber 0.0
      , GFXNumber 0.0
      , GFXNumber 64.0
      , GFXNumber 7.1771
      , GFXNumber 2.4414
      , GFXOperator GSSetTextMatrix
      , GFXNumber 0.0
      , GFXOperator GSSetCharacterSpacing
      , GFXOperator GSIntersectClippingPathEOR
      , GFXNumber 0.0
      , GFXOperator GSSetWordSpacing
      , GFXComment " Comment"
      , GFXNumber 1.0
      , GFXNumber 0.0
      , GFXNumber 0.0
      , GFXOperator GSSetNonStrokeRGBColorspace
      , GFXString "\001"
      , GFXOperator GSShowText
      , GFXOperator GSEndText
      , GFXInlineImage
        (mkDictionary
          [ ("W"  , GFXNumber 17.0)
          , ("H"  , GFXNumber 17.0)
          , ("CS" , GFXName "RGB")
          , ("BPC", GFXNumber 8.0)
          , ("F"  , mkGFXArray [GFXName "A85", GFXName "LZW"])
          ]
        )
        "ABCD"
      ]
    )
  , ( 1, "", mempty)
  , ( 2
    , "1 2 3 4 5 d0"
    , mkArray
        [ GFXNumber 1.0
        , GFXNumber 2.0
        , GFXNumber 3.0
        , GFXNumber 4.0
        , GFXNumber 5.0
        , GFXOperator GSSetGlyphWidth
        ]
    )
  , ( 3
    , "0.1 w q 6 7 8 9 10 re"
    , mkArray
        [ GFXNumber 0.1
        , GFXOperator GSSetLineWidth
        , GFXOperator GSSaveGS
        , GFXNumber 6.0
        , GFXNumber 7.0
        , GFXNumber 8.0
        , GFXNumber 9.0
        , GFXNumber 10.0
        , GFXOperator GSRectangle
        ]
    )
  ]

spec :: Spec
spec = describe "gfxParse" $ do
  forM_ streamExamples $ \(identifier, example, expected) -> do
    it ("should work with example " ++ show identifier)
      $          gfxParse example
      `shouldBe` Right expected
