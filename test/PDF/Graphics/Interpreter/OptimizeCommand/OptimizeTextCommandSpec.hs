module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeTextCommandSpec
  ( spec
  ) where

import Control.Monad (forM_)
import Control.Monad.State (State, evalState)

import Data.PDF.Command (mkCommand)
import Data.PDF.GFXObject
  ( GFXObject (GFXNumber, GFXString)
  , GSOperator (GSBeginText, GSEndText, GSSetCharacterSpacing, GSSetWordSpacing, GSShowText)
  )
import Data.PDF.InterpreterAction
  ( InterpreterAction (DeleteCommand, KeepCommand, ReplaceAndDeleteNextCommand, ReplaceCommand, SwitchCommand)
  )
import Data.PDF.InterpreterState (InterpreterState, defaultInterpreterState)
import Data.PDF.Program (Program, mkProgram)
import Data.Sequence (Seq (Empty, (:<|)), (<|), (|>))

import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeTextCommand
  (optimizeTextCommand)

import Test.Hspec (Spec, describe, it, shouldBe)

optimizeTextCommandExamples :: [(Int, Program, Program)]
optimizeTextCommandExamples =
  [ ( 0
    , mkProgram
        [ mkCommand GSBeginText []
        , mkCommand GSSetCharacterSpacing [ GFXNumber 0 ]
        , mkCommand GSSetWordSpacing [ GFXNumber 0 ]
        , mkCommand GSShowText [ GFXString "Hello, World!" ]
        , mkCommand GSEndText []
        ]
    , mkProgram
        [ mkCommand GSBeginText []
        , mkCommand GSShowText [ GFXString "Hello, World!" ]
        , mkCommand GSEndText []
        ]
    )
  , ( 1, mempty, mempty )
  , ( 2
    , mkProgram
        [ mkCommand GSBeginText []
        , mkCommand GSSetCharacterSpacing [ GFXNumber 10 ]
        , mkCommand GSShowText [ GFXString "A" ]
        , mkCommand GSEndText []
        , mkCommand GSBeginText []
        , mkCommand GSSetCharacterSpacing [ GFXNumber 0 ]
        , mkCommand GSShowText [ GFXString "B" ]
        , mkCommand GSEndText []
        ]
    , mkProgram
        [ mkCommand GSBeginText []
        , mkCommand GSSetCharacterSpacing [ GFXNumber 10 ]
        , mkCommand GSShowText [ GFXString "A" ]
        , mkCommand GSEndText []
        , mkCommand GSBeginText []
        , mkCommand GSSetCharacterSpacing [ GFXNumber 0 ]
        , mkCommand GSShowText [ GFXString "B" ]
        , mkCommand GSEndText []
        ]
    )
  , ( 3
    , mkProgram
        [ mkCommand GSBeginText []
        , mkCommand GSSetCharacterSpacing [ GFXNumber 10 ]
        , mkCommand GSShowText [ GFXString "A" ]
        , mkCommand GSEndText []
        , mkCommand GSBeginText []
        , mkCommand GSSetCharacterSpacing [ GFXNumber 10 ]
        , mkCommand GSShowText [ GFXString "B" ]
        , mkCommand GSEndText []
        ]
    , mkProgram
        [ mkCommand GSBeginText []
        , mkCommand GSSetCharacterSpacing [ GFXNumber 10 ]
        , mkCommand GSShowText [ GFXString "A" ]
        , mkCommand GSEndText []
        , mkCommand GSBeginText []
        , mkCommand GSShowText [ GFXString "B" ]
        , mkCommand GSEndText []
        ]
    )
  ]

optimizeTextCommands
  :: Program
  -> Program
  -> State InterpreterState Program
optimizeTextCommands program Empty = return program
optimizeTextCommands program (command :<| rest) =
  optimizeTextCommand command rest >>= \case
    KeepCommand -> optimizeTextCommands (program |> command) rest

    DeleteCommand -> optimizeTextCommands program rest
    ReplaceCommand optimizedCommand' ->
      optimizeTextCommands (program |> optimizedCommand') rest

    ReplaceAndDeleteNextCommand optimizedCommand' -> case rest of
      Empty -> return program
      (_commandToDelete :<| rest') ->
        optimizeTextCommands (program |> optimizedCommand') rest'

    SwitchCommand -> case rest of
      Empty -> return program
      (nextCommand :<| rest') ->
        optimizeTextCommands program (nextCommand <| command <| rest')

spec :: Spec
spec = do
  describe "optimizeTextCommand" $
    forM_ optimizeTextCommandExamples $ \(index, program, expected) -> do
      it ("should work with example " ++ show index)
        $ ( flip evalState defaultInterpreterState . optimizeTextCommands mempty) program
          `shouldBe` expected

