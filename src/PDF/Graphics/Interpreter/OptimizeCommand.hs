module PDF.Graphics.Interpreter.OptimizeCommand
  ( optimizeCommand
  ) where

import Control.Monad (foldM)
import Control.Monad.State (State)

import Data.PDF.Command (Command)
import Data.PDF.InterpreterAction (InterpreterAction (KeepCommand))
import Data.PDF.InterpreterState (InterpreterState)
import Data.PDF.Program (Program)

import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeColorCommand
    ( optimizeColorCommand
    )
import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeDrawCommand
    ( optimizeDrawCommand
    )
import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeGeneric
    ( optimizeGeneric
    )
import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeGraphicsMatrix
    ( optimizeGraphicsMatrix
    )
import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeStateCommand
    ( optimizeStateCommand
    )
import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeTextCommand
    ( optimizeTextCommand
    )
import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeTextMatrix
    ( optimizeTextMatrix
    )

optimizations :: [Command -> Program -> State InterpreterState InterpreterAction]
optimizations =
  [ optimizeStateCommand
  , optimizeGraphicsMatrix
  , optimizeTextMatrix
  , optimizeDrawCommand
  , optimizeTextCommand
  , optimizeColorCommand
  , optimizeGeneric
  ]

findAction
  :: Monad m
  => InterpreterAction
  -> m InterpreterAction
  -> m InterpreterAction
findAction KeepCommand action        = action
findAction action      _anythingElse = return action

{- |
The 'optimizeCommand' function takes a 'GraphicsState' and a 'Command' and
returns an optimized 'Command'.
-}
optimizeCommand
  :: Command
  -> Program
  -> State InterpreterState InterpreterAction
optimizeCommand command rest =
  foldM findAction KeepCommand (($) <$> optimizations <*> [command] <*> [rest])
