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
  , optimizeCommandPreservingText
  ) where

import Control.Monad.State (State)

import Data.PDF.Command (Command (cOperator))
import Data.PDF.OperatorCategory
  (OperatorCategory (TextPositioningOperator, TextShowingOperator, TextStateOperator), category)
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
optimizeCommand = runOptimizations optimizations

-- | Inherited or externally supplied text parameters are not represented by
-- the approximate interpreter's defaults. Leave text for the exact final pass;
-- retain the existing graphics-only optimizations and q/Q state handling.
optimizeCommandPreservingText
  :: Command
  -> Program
  -> State InterpreterState InterpreterAction
optimizeCommandPreservingText command rest
  | category (cOperator command) `elem`
      [ TextPositioningOperator
      , TextShowingOperator
      , TextStateOperator
      ]
  = pure KeepCommand

  | otherwise
  = runOptimizations
      [ optimizeOrder
      , optimizeStateCommand
      , optimizeGraphicsMatrix
      , optimizeDrawCommand
      , optimizeColorCommand
      , optimizeGeneric
      ] command rest

runOptimizations
  :: [Command -> Program -> State InterpreterState InterpreterAction]
  -> Command
  -> Program
  -> State InterpreterState InterpreterAction
runOptimizations passes command rest =
  go passes
 where
  go
    :: [Command -> Program -> State InterpreterState InterpreterAction]
    -> State InterpreterState InterpreterAction
  go [] = return KeepCommand
  go (optimization : remaining) =
    optimization command rest >>= \case
      KeepCommand -> go remaining
      action      -> return action
