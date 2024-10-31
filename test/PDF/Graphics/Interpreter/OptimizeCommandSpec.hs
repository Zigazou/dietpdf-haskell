module PDF.Graphics.Interpreter.OptimizeCommandSpec
  ( spec
  ) where

import Control.Monad (forM_)
import Control.Monad.State (evalState)

import Data.PDF.Command (Command, mkCommand)
import Data.PDF.GFXObject
  ( GFXObject (GFXNumber)
  , GSOperator (GSRestoreGS, GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace)
  )
import Data.PDF.InterpreterAction
  (InterpreterAction (KeepCommand, ReplaceCommand))
import Data.PDF.InterpreterState (defaultInterpreterState)
import Data.PDF.Program (Program, mkProgram)

import PDF.Graphics.Interpreter.OptimizeCommand (optimizeCommand)

import Test.Hspec (Spec, describe, it, shouldBe)

optimizeCommandExamples :: [(Command, Program, InterpreterAction)]
optimizeCommandExamples =
  [ ( mkCommand GSSetStrokeRGBColorspace [GFXNumber 0, GFXNumber 0, GFXNumber 0]
    , mempty
    , ReplaceCommand $ mkCommand GSSetStrokeGrayColorspace [GFXNumber 0]
    )
  , ( mkCommand GSSetNonStrokeRGBColorspace [GFXNumber 0, GFXNumber 0, GFXNumber 0]
    , mempty
    , ReplaceCommand $ mkCommand GSSetNonStrokeGrayColorspace [GFXNumber 0]
    )
  , ( mkCommand GSSetNonStrokeRGBColorspace [GFXNumber 1, GFXNumber 2, GFXNumber 3]
    , mempty
    , KeepCommand
    )
  , ( mkCommand GSRestoreGS mempty
    , mempty
    , KeepCommand
    )
  , ( mkCommand GSRestoreGS mempty
    , mkProgram [mkCommand GSRestoreGS mempty]
    , KeepCommand
    )
  ]

spec :: Spec
spec = do
  describe "optimizeCommand" $
    forM_ optimizeCommandExamples $ \(command, program, expected) -> do
      it ("should work with " ++ show command)
        $ evalState (optimizeCommand command program) defaultInterpreterState
            `shouldBe` expected
