module PDF.Graphics.Interpreter.OptimizeProgram.OptimizeIneffectiveSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.PDF.Command (mkCommand)
import Data.PDF.GFXObject
    ( GFXObject (GFXNumber)
    , GSOperator (GSEndPath, GSLineTo, GSMoveTo, GSRestoreGS, GSSaveGS, GSSetCTM, GSSetLineWidth)
    )
import Data.PDF.Program (Program, mkProgram)

import PDF.Graphics.Interpreter.OptimizeProgram.OptimizeIneffective
    ( optimizeIneffective
    )

import Test.Hspec (Spec, describe, it, shouldBe)


optimizeIneffectiveExamples :: [(Program, Program)]
optimizeIneffectiveExamples =
  [ (mempty, mempty)
  , ( mkProgram [mkCommand GSMoveTo [GFXNumber 1.000042, GFXNumber 2.421]]
    , mkProgram [mkCommand GSMoveTo [GFXNumber 1.000042, GFXNumber 2.421]]
    )
  , ( mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSSetCTM [ GFXNumber 0
                             , GFXNumber 0
                             , GFXNumber 0
                             , GFXNumber 0
                             , GFXNumber 0
                             , GFXNumber 0
                             ]
        , mkCommand GSRestoreGS []
        ]
    , mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSRestoreGS []
        ]
    )
    , ( mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSLineTo [ GFXNumber 1, GFXNumber 2 ]
        , mkCommand GSSetCTM [ GFXNumber 1
                             , GFXNumber 0
                             , GFXNumber 0
                             , GFXNumber 1
                             , GFXNumber 8.613
                             , GFXNumber (-10.192)
                             ]
        , mkCommand GSSetLineWidth [ GFXNumber 0.4 ]
        , mkCommand GSEndPath []
        , mkCommand GSRestoreGS []
        ]
    , mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSLineTo [ GFXNumber 1, GFXNumber 2 ]
        , mkCommand GSRestoreGS []
        ]
    )
  ]

spec :: Spec
spec = do
  describe "optimizeIneffective" $
    forM_ optimizeIneffectiveExamples $ \(example, expected) -> do
      it ("should work with " ++ show example)
        $          optimizeIneffective example
        `shouldBe` expected
