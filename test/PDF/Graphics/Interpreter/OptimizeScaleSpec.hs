module PDF.Graphics.Interpreter.OptimizeScaleSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.PDF.Command (mkCommand)
import Data.PDF.GFXObject
  ( GFXObject (GFXName, GFXNumber)
  , GSOperator (GSLineTo, GSMoveTo, GSPaintShapeColourShading, GSPaintXObject, GSRectangle, GSRestoreGS, GSSaveGS, GSSetCTM, GSSetLineWidth)
  )
import Data.PDF.Program (Program, mkProgram)

import PDF.Graphics.Interpreter.OptimizeScale (isScaleOptimizable)

import Test.Hspec (Spec, describe, it, shouldBe)

isScaleOptimizableExamples :: [(String, Program, Bool)]
isScaleOptimizableExamples =
  [ ( "True for an empty program"
    , mkProgram []
    , True
    )
  , ( "True for a program with only path construction commands"
    , mkProgram
        [ mkCommand GSMoveTo [GFXNumber 10, GFXNumber 20]
        , mkCommand GSLineTo [GFXNumber 30, GFXNumber 40]
        , mkCommand GSRectangle [GFXNumber 0, GFXNumber 0, GFXNumber 100, GFXNumber 100]
        ]
    , True
    )
  , ( "True for a program with graphics state commands"
    , mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSSetLineWidth [GFXNumber 2.5]
        , mkCommand GSSetCTM [GFXNumber 1, GFXNumber 0, GFXNumber 0, GFXNumber 1, GFXNumber 10, GFXNumber 20]
        , mkCommand GSRestoreGS []
        ]
    , True
    )
  ,( "False for a program containing GSPaintXObject"
    , mkProgram
        [ mkCommand GSMoveTo [GFXNumber 10, GFXNumber 20]
        , mkCommand GSPaintXObject [GFXName "Image1"]
        ]
    , False
    )
  , ( "False for a program with only GSPaintXObject"
    , mkProgram
        [ mkCommand GSPaintXObject [GFXName "Form1"]
        ]
    , False
    )

  , ( "False for a program containing GSPaintShapeColourShading"
    , mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSPaintShapeColourShading [GFXName "Sh1"]
        , mkCommand GSRestoreGS []
        ]
    , False
    )
  , ( "False for a program with only GSPaintShapeColourShading"
    , mkProgram
        [ mkCommand GSPaintShapeColourShading [GFXName "Shading1"]
        ]
    , False
    )
  , ( "False for a program containing both paint operators"
    , mkProgram
            [ mkCommand GSMoveTo [GFXNumber 0, GFXNumber 0]
            , mkCommand GSPaintXObject [GFXName "Image1"]
            , mkCommand GSLineTo [GFXNumber 100, GFXNumber 100]
            , mkCommand GSPaintShapeColourShading [GFXName "Sh1"]
            ]
    , False
    )
  , ( "False when GSPaintXObject appears at the end"
    , mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSSetCTM [GFXNumber 1, GFXNumber 0, GFXNumber 0, GFXNumber 1, GFXNumber 0, GFXNumber 0]
        , mkCommand GSMoveTo [GFXNumber 10, GFXNumber 20]
        , mkCommand GSLineTo [GFXNumber 30, GFXNumber 40]
        , mkCommand GSRestoreGS []
        , mkCommand GSPaintXObject [GFXName "Logo"]
        ]
    , False
    )
  , ( "False when GSPaintShapeColourShading appears at the beginning"
    , mkProgram
        [ mkCommand GSPaintShapeColourShading [GFXName "Background"]
        , mkCommand GSMoveTo [GFXNumber 10, GFXNumber 20]
        , mkCommand GSLineTo [GFXNumber 30, GFXNumber 40]
        ]
    , False
    )
  , ( "True for a complex program without paint operators"
    , mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSSetLineWidth [GFXNumber 1.0]
        , mkCommand GSMoveTo [GFXNumber 10, GFXNumber 20]
        , mkCommand GSLineTo [GFXNumber 100, GFXNumber 20]
        , mkCommand GSLineTo [GFXNumber 100, GFXNumber 100]
        , mkCommand GSLineTo [GFXNumber 10, GFXNumber 100]
        , mkCommand GSRectangle [GFXNumber 50, GFXNumber 50, GFXNumber 30, GFXNumber 30]
        , mkCommand GSSetCTM [GFXNumber 1, GFXNumber 0, GFXNumber 0, GFXNumber 1, GFXNumber 5, GFXNumber 5]
        , mkCommand GSRestoreGS []
        ]
    , True
    )
  ]

spec :: Spec
spec = do
  describe "isScaleOptimizable"
    $ forM_ isScaleOptimizableExamples
    $ \(message, example, expected) -> do
        it ("should return " ++ message)
          $ isScaleOptimizable example `shouldBe` expected
