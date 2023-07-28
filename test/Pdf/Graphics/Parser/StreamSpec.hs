module Pdf.Graphics.Parser.StreamSpec
  ( spec
  ) where

import           Test.Hspec                     ( describe
                                                , it
                                                , Spec
                                                , shouldBe
                                                )

import qualified Data.ByteString               as BS
import           Pdf.Graphics.Parser.Stream     ( gfxParse )
import           Control.Monad                  ( forM_ )
import           Pdf.Graphics.Object            ( GFXObject
                                                  ( GFXString
                                                  , GFXOperator
                                                  , GFXNumber
                                                  , GFXString
                                                  , GFXComment
                                                  , GFXName
                                                  , GFXInlineImage
                                                  )
                                                , GSOperator
                                                  ( GSBeginText
                                                  , GSEndText
                                                  , GSSetTextFont
                                                  , GSSetTextMatrix
                                                  , GSSetCharacterSpacing
                                                  , GSSetWordSpacing
                                                  , GSSetNonStrokeRGBColorspace
                                                  , GSShowText
                                                  , GSIntersectClippingPathEOR
                                                  )
                                                , mkGFXArray
                                                )
import           Util.Dictionary                ( mkDictionary )
import           Util.Array                     ( Array
                                                , mkArray
                                                )

streamExamples :: [(BS.ByteString, Array GFXObject)]
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
