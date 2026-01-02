{-|
Command-level optimization orchestration for PDF graphics

Coordinates multiple optimization passes over individual PDF graphics commands.

This module applies a pipeline of optimization strategies to each command in
sequence, accumulating the effect of all applicable optimizations. Each
optimization function can return either a modification action (delete, replace)
or a keep action if no optimization applies.

The optimization pipeline includes:
* Reordering operators for better optimization opportunities
* Removing redundant graphics state commands
* Simplifying transformation matrices
* Optimizing text positioning matrices
* Simplifying drawing paths
* Optimizing text operations
* Removing redundant color settings
* Generic precision reduction
-}
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
  (optimizeColorCommand)
import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeDrawCommand
  (optimizeDrawCommand)
import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeGeneric
  (optimizeGeneric)
import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeGraphicsMatrix
  (optimizeGraphicsMatrix)
import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeOrder (optimizeOrder)
import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeStateCommand
  (optimizeStateCommand)
import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeTextCommand
  (optimizeTextCommand)
import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeTextMatrix
  (optimizeTextMatrix)

{-|
Pipeline of optimization functions applied to each command.

Each function takes a command and the remaining program, and returns an action
(either keeping the command or replacing/deleting it) based on the current
interpreter state.

The order of optimizations matters: earlier passes may enable later
optimizations or may depend on certain conditions established by previous
passes.
-}
optimizations :: [Command -> Program -> State InterpreterState InterpreterAction]
optimizations =
  [ optimizeOrder
  , optimizeStateCommand
  , optimizeGraphicsMatrix
  , optimizeTextMatrix
  , optimizeDrawCommand
  , optimizeTextCommand
  , optimizeColorCommand
  , optimizeGeneric
  ]

{-|
Fold combinator for optimization actions.

Propagates the first non-trivial optimization action through a series of
alternative optimizations. Once any optimization returns a meaningful action
(other than @KeepCommand@), stops processing and returns that action. Otherwise,
evaluates the next optimization in the pipeline.

Used to combine multiple optimization functions where the first applicable one
takes precedence.
-}
findAction
  :: Monad m
  => InterpreterAction
  -> m InterpreterAction
  -> m InterpreterAction
findAction KeepCommand     nextActionM   = nextActionM
findAction effectiveAction _anythingElse = return effectiveAction

{-|
Apply all optimization passes to a single command.

Runs the command through the optimization pipeline, allowing each optimization
function to examine the command and the remaining program, then decide whether
to keep, modify, or delete the command based on the current graphics interpreter
state.

Optimizations are applied in sequence, and the first one that produces a
meaningful action (other than @KeepCommand@) determines the result. The
optimization state is threaded through all applications via the State monad.

@param command@ the command to optimize @param rest@ the remaining program after
this command @return@ an action indicating whether to keep, replace, or delete
the command
-}
optimizeCommand
  :: Command
  -> Program
  -> State InterpreterState InterpreterAction
optimizeCommand command rest =
  foldM findAction KeepCommand (($) <$> optimizations <*> [command] <*> [rest])
