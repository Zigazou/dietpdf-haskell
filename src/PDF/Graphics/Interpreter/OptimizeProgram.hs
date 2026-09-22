{-|
Program-level optimization orchestration for PDF graphics

Coordinates all optimization passes over PDF graphics programs, applying both
command-level optimizations and program-wide structural optimizations.

The optimization pipeline includes:

* Program-wide structural optimizations:
  - Removing useless save/restore pairs
  - Converting line paths to rectangle operators
  - Removing ineffective (empty) operations
  - Eliminating duplicated consecutive operators
  - Removing redundant marked content sequences

* Command-level optimizations:
  - Reordering operators for optimization opportunities
  - Removing redundant graphics state settings
  - Simplifying transformation matrices
  - Optimizing text operations and positioning
  - Simplifying drawing paths
  - Removing redundant color specifications
  - Generic precision reduction

Optimizations are applied iteratively until convergence (no further changes).
-}
module PDF.Graphics.Interpreter.OptimizeProgram ( optimizeProgram )
where

import Control.Monad.State (State, evalState)

import Data.PDF.InterpreterAction
  ( InterpreterAction (DeleteCommand, KeepCommand, ReplaceAndDeleteNextCommand, ReplaceCommand, SwitchCommand)
  )
import Data.PDF.InterpreterState
  ( InterpreterState (iWorkData)
  , consumeColorOpS
  , defaultInterpreterState
  , initRemainingColorOpsS
  )
import Data.PDF.Program (Program, programComputedSize)
import Data.PDF.WorkData (WorkData)
import Data.Sequence (Seq (Empty, (:<|)), (<|), (|>))

import PDF.Graphics.Interpreter.OptimizeCommand (optimizeCommand)
import PDF.Graphics.Interpreter.OptimizeProgram.OptimizeDuplicates
  (optimizeDuplicates)
import PDF.Graphics.Interpreter.OptimizeProgram.OptimizeIneffective
  (optimizeIneffective)
import PDF.Graphics.Interpreter.OptimizeProgram.OptimizeMarkedContent
  (optimizeMarkedContent)
import PDF.Graphics.Interpreter.OptimizeProgram.OptimizeMergeableTextCommands
  (optimizeMergeableTextCommands)
import PDF.Graphics.Interpreter.OptimizeProgram.OptimizeRectangle
  (optimizeRectangle)
import PDF.Graphics.Interpreter.OptimizeProgram.OptimizeSaveRestore
  (optimizeSaveRestore)

import Util.Transform (untilNoImprovement)

{-|
Apply command-level optimizations to a program.

Processes a program sequentially, applying the optimization pipeline to each
command in turn. The current program is accumulated in the first parameter,
while commands to be processed are in the second parameter.

Each command goes through the optimization system and returns an action
indicating whether to keep, delete, replace, or reorder it. The accumulated
result respects these actions:

* @KeepCommand@ - add the command to the output program as-is
* @DeleteCommand@ - skip the command
* @ReplaceCommand cmd@ - add the optimized command to the output
* @ReplaceAndDeleteNextCommand cmd@ - replace current command and skip the next
  one
* @SwitchCommand@ - swap the current command with the next one (for reordering)

@param program@ the accumulated optimized program so far @param rest@ the
remaining commands to process @return@ the fully optimized program

Assumes 'Data.PDF.InterpreterState.initRemainingColorOpsS' has already been run
on the full input program, so that color commands consumed here keep the
remaining-color-op counters in sync (see 'optimizeCommandsFromStart').
-}
optimizeCommands
  :: Program
  -> Program
  -> State InterpreterState Program
optimizeCommands program Empty = return program
optimizeCommands program (command :<| rest) =
  optimizeCommand command rest >>= \case
    KeepCommand -> do
      consumeColorOpS command
      optimizeCommands (program |> command) rest

    DeleteCommand -> do
      consumeColorOpS command
      optimizeCommands program rest

    ReplaceCommand optimizedCommand' -> do
      consumeColorOpS command
      optimizeCommands (program |> optimizedCommand') rest

    ReplaceAndDeleteNextCommand optimizedCommand' -> case rest of
      Empty -> return program
      (commandToDelete :<| rest') -> do
        consumeColorOpS command
        consumeColorOpS commandToDelete
        optimizeCommands (program |> optimizedCommand') rest'

    SwitchCommand -> case rest of
      Empty -> return program
      (nextCommand :<| rest') ->
        optimizeCommands program (nextCommand <| command <| rest')

{-|
Run 'optimizeCommands' over the whole @program@, first initializing the
remaining-color-op counters used by 'PDF.Graphics.Interpreter.OptimizeCommand.
OptimizeColorCommand.optimizeColorCommand'.
-}
optimizeCommandsFromStart :: Program -> State InterpreterState Program
optimizeCommandsFromStart program = do
  initRemainingColorOpsS program
  optimizeCommands mempty program

{-|
Perform one complete optimization pass over a program.


Applies all program-wide structural optimizations followed by all command-level
optimizations:

Program-wide passes:
- Remove useless save/restore pairs
- Convert line paths to rectangle operators
- Remove empty/ineffective operations
- Remove duplicated consecutive operators
- Remove redundant marked content sequences
- Merge consecutive text display commands

Then applies command-level optimizations via @optimizeCommands@ to each command
in the optimized structure.

Keep these structural passes inside the fixed point: command removal can expose
empty scopes, duplicate settings, adjacent text displays and marked-content
wrappers. Coordinate rounding and path simplification can also expose rectangle
patterns. Idempotence of an isolated pass does not imply that it can be hoisted
out of this pipeline. Resource renaming and scale selection belong outside this
loop; ExtGState factorization runs after convergence.

@param workData@ the PDF work context containing document information @param
program@ the graphics program to optimize in one pass @return@ the result after
all optimizations in a single pass
-}
optimizeProgramOnePass :: WorkData -> Program -> Program
optimizeProgramOnePass workData
  = ( flip evalState defaultInterpreterState { iWorkData = workData }
    . optimizeCommandsFromStart
    )
  . optimizeMarkedContent
  . optimizeMergeableTextCommands
  . optimizeDuplicates
  . optimizeIneffective
  . optimizeRectangle
  . optimizeSaveRestore

{-|
Optimize a PDF graphics program through iterative refinement.

Repeatedly applies program-wide optimizations until no further changes are
possible (fixed point). This ensures that optimizations which create
opportunities for subsequent optimizations are fully exploited.

For example, removing a save/restore pair might expose duplicate operators that
can then be eliminated, which in turn might create new opportunities for
optimization.

If the result is empty (all commands removed), returns the original program
unchanged to preserve the document's semantics.

@param workData@ the PDF work context containing document information @param
program@ the graphics program to optimize @return@ the fully optimized program
-}
optimizeProgram :: WorkData -> Program -> Program
optimizeProgram workData program =
  let optimized = untilNoImprovement 4
                                     programComputedSize
                                     (optimizeProgramOnePass workData)
                                     program
  in
    if optimized == mempty
      then program
      else optimized
