module PDF.Graphics.Interpreter.OptimizeProgram ( optimizeProgram )
where

import Control.Monad.State (State, evalState)

import Data.PDF.InterpreterAction
    ( InterpreterAction (DeleteCommand, KeepCommand, ReplaceAndDeleteNextCommand, ReplaceCommand)
    )
import Data.PDF.InterpreterState
    ( InterpreterState (iWorkData)
    , defaultInterpreterState
    )
import Data.PDF.Program (Program)
import Data.PDF.WorkData (WorkData)
import Data.Sequence (Seq (Empty, (:<|)), (|>))

import PDF.Graphics.Interpreter.OptimizeCommand (optimizeCommand)
import PDF.Graphics.Interpreter.OptimizeProgram.OptimizeDuplicates
    ( optimizeDuplicates
    )
import PDF.Graphics.Interpreter.OptimizeProgram.OptimizeIneffective
    ( optimizeIneffective
    )
import PDF.Graphics.Interpreter.OptimizeProgram.OptimizeRectangle
    ( optimizeRectangle
    )
import PDF.Graphics.Interpreter.OptimizeProgram.OptimizeSaveRestore
    ( optimizeSaveRestore
    )

import Util.Transform (untilNoChange)

optimizeCommands
  :: Program
  -> Program
  -> State InterpreterState Program
optimizeCommands program Empty = return program
optimizeCommands program (command :<| rest) =
  optimizeCommand command rest >>= \case
    KeepCommand -> optimizeCommands (program |> command) rest
    DeleteCommand -> optimizeCommands program rest
    ReplaceCommand optimizedCommand' ->
      optimizeCommands (program |> optimizedCommand') rest
    ReplaceAndDeleteNextCommand optimizedCommand' -> case rest of
      Empty -> return program
      (_commandToDelete :<| rest') ->
        optimizeCommands (program |> optimizedCommand') rest'

optimizeProgramOnePass :: WorkData -> Program -> Program
optimizeProgramOnePass workData
  = (flip evalState defaultInterpreterState { iWorkData = workData }
  . optimizeCommands mempty)
  . optimizeDuplicates
  . optimizeIneffective
  . optimizeRectangle
  . optimizeSaveRestore

{- |
The 'optimizeProgram' function takes a 'Program' and returns an optimized
'Program'.
-}
optimizeProgram :: WorkData -> Program -> Program
optimizeProgram workData = untilNoChange (optimizeProgramOnePass workData)
