module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeColorSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.PDF.Command (Command, mkCommand)
import Data.PDF.GFXObject
  ( GFXObject (GFXNumber)
  , GSOperator (GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace)
  )

import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeColor (optimizeColor)

import Test.Hspec (Spec, describe, it, shouldBe)

optimizeColorExamples :: [(Command, Command)]
optimizeColorExamples =
  [ ( mkCommand GSSetStrokeRGBColorspace [GFXNumber 0, GFXNumber 0, GFXNumber 0]
    , mkCommand GSSetStrokeGrayColorspace [GFXNumber 0]
    )
  , ( mkCommand GSSetStrokeRGBColorspace [GFXNumber 1, GFXNumber 2, GFXNumber 3]
    , mkCommand GSSetStrokeRGBColorspace [GFXNumber 1, GFXNumber 2, GFXNumber 3]
    )
  , ( mkCommand GSSetNonStrokeRGBColorspace [GFXNumber 0, GFXNumber 0, GFXNumber 0]
    , mkCommand GSSetNonStrokeGrayColorspace [GFXNumber 0]
    )
  , ( mkCommand GSSetNonStrokeRGBColorspace [GFXNumber 1, GFXNumber 2, GFXNumber 3]
    , mkCommand GSSetNonStrokeRGBColorspace [GFXNumber 1, GFXNumber 2, GFXNumber 3]
    )
  , ( mkCommand GSSetStrokeRGBColorspace [GFXNumber 1, GFXNumber 1, GFXNumber 1]
    , mkCommand GSSetStrokeGrayColorspace [GFXNumber 1]
    )
  , ( mkCommand GSSetNonStrokeRGBColorspace [GFXNumber 0.5, GFXNumber 0.5, GFXNumber 0.5]
    , mkCommand GSSetNonStrokeGrayColorspace [GFXNumber 0.5]
    )
  ]

spec :: Spec
spec = do
  describe "optimizeColor" $
    forM_ optimizeColorExamples $ \(example, expected) -> do
      it ("should work with " ++ show example)
        $          optimizeColor example
        `shouldBe` expected
