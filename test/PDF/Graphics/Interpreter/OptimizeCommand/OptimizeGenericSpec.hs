module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeGenericSpec
  ( spec
  ) where

import Control.Monad (forM_)
import Control.Monad.State (evalState)

import Data.PDF.Command (Command, mkCommand)
import Data.PDF.GFXObject
  ( GFXObject (GFXNumber)
  , GSOperator (GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace)
  )
import Data.PDF.InterpreterAction
  (InterpreterAction (KeepCommand, ReplaceCommand))
import Data.PDF.InterpreterState (defaultInterpreterState)
import Data.PDF.Program (Program)

import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeGeneric
  (optimizeGeneric)

import Test.Hspec (Spec, describe, it, shouldBe)

optimizeGenericExamples :: [(Command, Program, InterpreterAction)]
optimizeGenericExamples =
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
  ]

spec :: Spec
spec = do
  describe "optimizeGeneric" $
    forM_ optimizeGenericExamples $ \(command, program, expected) -> do
      it ("should work with " ++ show command)
        $ evalState (optimizeGeneric command program) defaultInterpreterState
            `shouldBe` expected
