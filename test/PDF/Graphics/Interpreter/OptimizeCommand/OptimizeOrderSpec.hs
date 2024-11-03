module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeOrderSpec
  ( spec
  ) where

import Control.Monad (forM_)
import Control.Monad.State (evalState)

import Data.PDF.Command (Command, mkCommand)
import Data.PDF.GFXObject
  (GSOperator (GSSetMiterLimit, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace))
import Data.PDF.InterpreterAction (InterpreterAction (SwitchCommand, KeepCommand))
import Data.PDF.InterpreterState (defaultInterpreterState)
import Data.PDF.Program (Program, mkProgram)

import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeOrder (optimizeOrder)

import Test.Hspec (Spec, describe, it, shouldBe)

optimizeOrderExamples :: [(Command, Program, InterpreterAction)]
optimizeOrderExamples =
  [ ( mkCommand GSSetMiterLimit []
    , mkProgram [mkCommand GSSetStrokeGrayColorspace []]
    , SwitchCommand
    )
  , ( mkCommand GSSetStrokeGrayColorspace []
    , mkProgram [mkCommand GSSetStrokeRGBColorspace []]
    , KeepCommand
    )
  ]

spec :: Spec
spec = do
  describe "optimizeOrder" $
    forM_ optimizeOrderExamples $ \(command, program, expected) -> do
      it ("should work with " ++ show command)
        $ evalState (optimizeOrder command program) defaultInterpreterState
            `shouldBe` expected
