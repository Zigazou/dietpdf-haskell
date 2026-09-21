module PDF.Graphics.Interpreter.OptimizeProgram.OptimizeIneffectiveSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.PDF.Command (mkCommand)
import Data.PDF.GFXObject
  ( GFXObject (GFXName, GFXNumber)
  , GSOperator (GSBeginText, GSEndText, GSBeginMarkedContentSequencePL, GSEndPath, GSFillPathNZWR, GSLineTo, GSMoveTo, GSRestoreGS, GSSaveGS, GSSetCTM, GSSetLineWidth)
  )
import Data.PDF.Program (Program, mkProgram)

import PDF.Graphics.Interpreter.OptimizeProgram.OptimizeIneffective
  (anyPaintingCommandBeforeRestore, optimizeIneffective)

import Data.Sequence qualified as SQ
import Test.QuickCheck (elements, forAll, listOf, property)

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
  describe "optimizeIneffective" $ do
    it "matches suffix lookahead, including nested and unbalanced scopes" $
      property $ forAll (listOf (elements
        [GSSaveGS, GSRestoreGS, GSLineTo, GSFillPathNZWR,
         GSBeginText, GSEndText, GSBeginMarkedContentSequencePL])) $ \operators ->
        let program = mkProgram [mkCommand operator [] | operator <- operators]
            reference :: Program -> Program
            reference SQ.Empty = SQ.Empty
            reference (a SQ.:<| b SQ.:<| rest)
              | a == mkCommand GSBeginText [] && b == mkCommand GSEndText [] =
                  reference rest
            reference (command SQ.:<| rest)
              | command `elem` [mkCommand operator [] | operator <-
                  [GSSaveGS, GSRestoreGS, GSFillPathNZWR, GSBeginText,
                   GSEndText, GSBeginMarkedContentSequencePL]]
                  || anyPaintingCommandBeforeRestore 0 rest =
                      command SQ.<| reference rest
              | otherwise = reference rest
        in optimizeIneffective program == reference program

    it "preserves a long path ending in a painting command" $ do
      let program = SQ.replicate 30000 (mkCommand GSLineTo [GFXNumber 1, GFXNumber 2])
                    SQ.|> mkCommand GSFillPathNZWR []
      optimizeIneffective program `shouldBe` program

    it "removes a long path without a painting command" $ do
      let program = SQ.replicate 30000 (mkCommand GSLineTo [GFXNumber 1, GFXNumber 2])
      optimizeIneffective program `shouldBe` mempty

    forM_ optimizeIneffectiveExamples $ \(example, expected) -> do
      it ("should work with " ++ show example)
        $          optimizeIneffective example
        `shouldBe` expected

  describe "anyPaintingCommandBeforeRestore" $
    forM_ anyPaintingCommandBeforeRestoreExamples $ \(level, example, expected) -> do
      it ("should work with " ++ show example)
        $          anyPaintingCommandBeforeRestore level example
        `shouldBe` expected
