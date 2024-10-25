module PDF.Graphics.Parser.StreamSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.Array (mkArray)
import Data.ByteString (ByteString)
import Data.PDF.GFXObject
    ( GFXObject (GFXComment, GFXInlineImage, GFXName, GFXNumber, GFXOperator, GFXString, GFXString)
    , GSOperator (GSBeginText, GSEndText, GSIntersectClippingPathEOR, GSSetCharacterSpacing, GSSetNonStrokeRGBColorspace, GSSetTextFont, GSSetTextMatrix, GSSetWordSpacing, GSShowText)
    , mkGFXArray
    )
import Data.PDF.GFXObjects (GFXObjects)

import PDF.Graphics.Parser.Stream (gfxParse)

import Test.Hspec (Spec, describe, it, shouldBe)

import Util.Dictionary (mkDictionary)

streamExamples :: [(ByteString, GFXObjects)]
streamExamples =
  [ ( "BT /F1 1 Tf 64 0 0 64 7.1771 2.4414 Tm 0 Tc \
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
  ]

spec :: Spec
spec = describe "gfxParse" $ do
  forM_ streamExamples $ \(example, expected) -> do
    it ("should work with " ++ show example)
      $          gfxParse example
      `shouldBe` Right expected
