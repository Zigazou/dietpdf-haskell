module PDF.Graphics.Interpreter.OptimizeProgram.OptimizeIneffectiveSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.PDF.Command (mkCommand)
import Data.PDF.GFXObject
  ( GFXObject (GFXName, GFXNumber)
  , GSOperator (GSBeginMarkedContentSequencePL, GSEndPath, GSFillPathNZWR, GSLineTo, GSMoveTo, GSRestoreGS, GSSaveGS, GSSetCTM, GSSetLineWidth)
  )
import Data.PDF.Program (Program, mkProgram)

import PDF.Graphics.Interpreter.OptimizeProgram.OptimizeIneffective
  (anyPaintingCommandBeforeRestore, optimizeIneffective)

import Test.Hspec (Spec, describe, it, shouldBe)


optimizeIneffectiveExamples :: [(Program, Program)]
optimizeIneffectiveExamples =
  [ (mempty, mempty)
  , ( mkProgram [mkCommand GSMoveTo [GFXNumber 1.000042, GFXNumber 2.421]]
    , mempty
    )
  , ( mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSSetCTM []
        , mkCommand GSRestoreGS []
        ]
    , mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSRestoreGS []
        ]
    )
  , ( mkProgram [ mkCommand GSSetCTM [] ]
    , mempty
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
    )
  , ( mkProgram
        [ mkCommand GSLineTo [ GFXNumber 24, GFXNumber (-1)]
        , mkCommand GSLineTo [ GFXNumber 26, GFXNumber (-1)]
        , mkCommand GSFillPathNZWR []
        , mkCommand GSRestoreGS []
        , mkCommand GSBeginMarkedContentSequencePL [GFXName "a"]
        , mkCommand GSBeginMarkedContentSequencePL [GFXName "b"]
        , mkCommand GSRestoreGS []
        , mkCommand GSSaveGS []
        ]
    , mkProgram
        [ mkCommand GSLineTo [ GFXNumber 24, GFXNumber (-1)]
        , mkCommand GSLineTo [ GFXNumber 26, GFXNumber (-1)]
        , mkCommand GSFillPathNZWR []
        , mkCommand GSRestoreGS []
        , mkCommand GSBeginMarkedContentSequencePL [GFXName "a"]
        , mkCommand GSBeginMarkedContentSequencePL [GFXName "b"]
        , mkCommand GSRestoreGS []
        , mkCommand GSSaveGS []
        ]
    )
  ]

anyPaintingCommandBeforeRestoreExamples :: [(Int, Program, Bool)]
anyPaintingCommandBeforeRestoreExamples =
  [ (0, mempty, False)
  , (0, mkProgram [mkCommand GSMoveTo [GFXNumber 1.000042, GFXNumber 2.421]], False)
  , (0, mkProgram [mkCommand GSSetCTM []], False)
  , (0, mkProgram [mkCommand GSSaveGS [], mkCommand GSSetCTM [], mkCommand GSRestoreGS []], False)
  , (0, mkProgram [mkCommand GSSetCTM [], mkCommand GSRestoreGS []], False)
  , (0, mkProgram [mkCommand GSSaveGS [], mkCommand GSLineTo [GFXNumber 1, GFXNumber 2], mkCommand GSSetCTM [GFXNumber 1, GFXNumber 0, GFXNumber 0, GFXNumber 1, GFXNumber 8.613, GFXNumber (-10.192)], mkCommand GSSetLineWidth [GFXNumber 0.4], mkCommand GSEndPath [], mkCommand GSRestoreGS []], True)
  , (0, mkProgram [mkCommand GSSetCTM []], False)
  , (0, mkProgram [mkCommand GSLineTo [GFXNumber 24, GFXNumber (-1)], mkCommand GSLineTo [GFXNumber 26, GFXNumber (-1)], mkCommand GSFillPathNZWR [], mkCommand GSRestoreGS [], mkCommand GSBeginMarkedContentSequencePL [GFXName "a"], mkCommand GSBeginMarkedContentSequencePL [GFXName "b"], mkCommand GSRestoreGS [], mkCommand GSSaveGS []], True)
  ]

spec :: Spec
spec = do
  describe "optimizeIneffective" $
    forM_ optimizeIneffectiveExamples $ \(example, expected) -> do
      it ("should work with " ++ show example)
        $          optimizeIneffective example
        `shouldBe` expected

  describe "anyPaintingCommandBeforeRestore" $
    forM_ anyPaintingCommandBeforeRestoreExamples $ \(level, example, expected) -> do
      it ("should work with " ++ show example)
        $          anyPaintingCommandBeforeRestore level example
        `shouldBe` expected
