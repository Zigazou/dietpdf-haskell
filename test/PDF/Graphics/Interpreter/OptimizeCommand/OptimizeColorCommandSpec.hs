module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeColorCommandSpec
  ( spec
  ) where

import Control.Monad (forM_)
import Control.Monad.State (State, evalState)

import Data.PDF.Command (mkCommand)
import Data.PDF.GFXObject
  ( GFXObject (GFXNumber)
  , GSOperator (GSFillPathNZWR, GSRectangle, GSSetNonStrokeCMYKColorspace, GSSetNonStrokeGrayColorspace)
  )
import Data.PDF.InterpreterAction
  ( InterpreterAction (DeleteCommand, KeepCommand, ReplaceAndDeleteNextCommand, ReplaceCommand, SwitchCommand)
  )
import Data.PDF.InterpreterState (InterpreterState, defaultInterpreterState)
import Data.PDF.Program (Program, mkProgram)
import Data.Sequence (Seq (Empty, (:<|)), (<|), (|>))

import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeColorCommand
  (optimizeColorCommand)

import Test.Hspec (Spec, describe, it, shouldBe)

optimizeColorCommandExamples :: [(Int, Program, Program)]
optimizeColorCommandExamples =
  [ ( 0
    , mkProgram
        [ mkCommand GSSetNonStrokeGrayColorspace [GFXNumber 1]
        , mkCommand GSRectangle [ GFXNumber 5, GFXNumber 5, GFXNumber 50, GFXNumber 10]
        , mkCommand GSFillPathNZWR []
        , mkCommand GSSetNonStrokeCMYKColorspace [GFXNumber 0, GFXNumber 0, GFXNumber 0, GFXNumber 1]
        , mkCommand GSRectangle [ GFXNumber 5, GFXNumber 5, GFXNumber 50, GFXNumber 10]
        , mkCommand GSFillPathNZWR []
        , mkCommand GSSetNonStrokeGrayColorspace [GFXNumber 1]
        , mkCommand GSRectangle [ GFXNumber 0, GFXNumber 0, GFXNumber 50, GFXNumber 10]
        , mkCommand GSFillPathNZWR []
        ]
    , mkProgram
        [ mkCommand GSSetNonStrokeGrayColorspace [GFXNumber 1]
        , mkCommand GSRectangle [ GFXNumber 5, GFXNumber 5, GFXNumber 50, GFXNumber 10]
        , mkCommand GSFillPathNZWR []
        , mkCommand GSSetNonStrokeCMYKColorspace [GFXNumber 0, GFXNumber 0, GFXNumber 0, GFXNumber 1]
        , mkCommand GSRectangle [ GFXNumber 5, GFXNumber 5, GFXNumber 50, GFXNumber 10]
        , mkCommand GSFillPathNZWR []
        , mkCommand GSSetNonStrokeGrayColorspace [GFXNumber 1]
        , mkCommand GSRectangle [ GFXNumber 0, GFXNumber 0, GFXNumber 50, GFXNumber 10]
        , mkCommand GSFillPathNZWR []
        ]
    )
    , (1, mempty, mempty)
  ]

optimizeColorCommands
  :: Program
  -> Program
  -> State InterpreterState Program
optimizeColorCommands program Empty = return program
optimizeColorCommands program (command :<| rest) =
  optimizeColorCommand command rest >>= \case
    KeepCommand -> optimizeColorCommands (program |> command) rest

    DeleteCommand -> optimizeColorCommands program rest

    ReplaceCommand optimizedCommand' ->
      optimizeColorCommands (program |> optimizedCommand') rest

    ReplaceAndDeleteNextCommand optimizedCommand' -> case rest of
      Empty -> return program
      (_commandToDelete :<| rest') ->
        optimizeColorCommands (program |> optimizedCommand') rest'

    SwitchCommand -> case rest of
      Empty -> return program
      (nextCommand :<| rest') ->
        optimizeColorCommands program (nextCommand <| command <| rest')

spec :: Spec
spec = do
  describe "optimizeColorCommand" $
    forM_ optimizeColorCommandExamples $ \(index, program, expected) -> do
      it ("should work with example " ++ show index)
        $ ( flip evalState defaultInterpreterState . optimizeColorCommands mempty) program
          `shouldBe` expected

