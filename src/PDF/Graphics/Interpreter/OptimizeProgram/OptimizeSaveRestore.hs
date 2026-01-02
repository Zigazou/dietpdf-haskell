{-|
Optimization of save/restore graphics state commands

Optimizes PDF graphics programs by removing useless save/restore command pairs.

Save (q) and restore (Q) commands manage the graphics state stack. Pairs of
these commands that don't enclose any state-modifying operations can be
eliminated. This module detects and removes:

* Consecutive restore commands (indicating empty save/restore pairs)
* Save/restore pairs with only marked content operators between them
* Save/restore pairs with no content between them

The optimization maintains balance and correctness of the graphics state stack.
-}
module PDF.Graphics.Interpreter.OptimizeProgram.OptimizeSaveRestore
  ( optimizeSaveRestore
  , findRelatedSave
  )
where


import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
  ( GSOperator (GSBeginMarkedContentSequence, GSBeginMarkedContentSequencePL, GSEndMarkedContentSequence, GSRestoreGS, GSSaveGS)
  )
import Data.PDF.Program (Program)
import Data.Sequence (Seq (Empty, (:<|), (:|>)), breakl, singleton, (<|))

import Util.Transform (untilNoChange)

{-|
Test if a command is a restore graphics state operator.

Checks whether the given command is a GSRestoreGS (Q) operator that restores the
previously saved graphics state.
-}
onRestore :: Command -> Bool
onRestore (Command GSRestoreGS _params) = True
onRestore _anyOtherCommand              = False

{-|
Pattern match for restore graphics state operator (Q).

Matches a restore command with no additional parameters.
-}
pattern Restore :: Command
pattern Restore = Command GSRestoreGS Empty

{-|
Pattern match for save graphics state operator (q).

Matches a save command with no additional parameters.
-}
pattern Save :: Command
pattern Save = Command GSSaveGS Empty

{-|
Find the save command associated with a restore command.

Starts from the end of the program and looks backward for the corresponding save
command that matches the restore. The search respects nesting levels, counting
save and restore operators to find the proper pair.

The restore command must not be included in the input program; the function
assumes the restore has already been removed.

Returns @Just (beforeSave, afterSave)@ if found, where:

* @beforeSave@ is the program before the matched save command
* @afterSave@ is the program after the matched save command (excluding the save
  itself)

Returns @Nothing@ if no matching save command can be found (unbalanced
structure).
-}
findRelatedSave :: Program -> Maybe (Program, Program)
findRelatedSave program = go 1 (Just (program, mempty))
 where
  go :: Int -> Maybe (Program, Program) -> Maybe (Program, Program)
  go 0 result = result

  go level (Just (before :|> Restore, after)) =
    go (level + 1) (Just (before, Restore <| after))

  go level (Just (before :|> Save, after)) =
    go (level - 1) (Just (before, Save <| after))

  go level (Just (before :|> command, after)) =
      go level (Just (before, command <| after))

  go level (Just (command :<| Empty, after)) =
      go level (Just (mempty, command <| after))

  go _anyOtherLevel _anyOtherValue = Nothing

{-|
Find two consecutive restore commands in the program.

Searches for a pattern of two adjacent restore commands (GSRestoreGS operators).
This pattern indicates an empty save/restore pair that can be optimized away.

Returns @Just (beforeDoubleRestore, afterDoubleRestore)@ if found, where:

* @beforeDoubleRestore@ is everything before the first restore
* @afterDoubleRestore@ is everything after the second restore

Recursively searches deeper nesting levels if the first restore found is not
immediately followed by another restore.

Returns @Nothing@ if no double restore pattern is found.
-}
findDoubleRestore :: Program -> Maybe (Program, Program)
findDoubleRestore program = case breakl onRestore program of
  (beforeRestore, Restore :<| Restore :<| afterRestore) ->
    Just (beforeRestore, afterRestore)
  (beforeRestore, Restore :<| afterRestore) -> do
    (beforeRestore', afterRestore') <- findDoubleRestore afterRestore
    Just (beforeRestore <> singleton Restore <> beforeRestore', afterRestore')
  _anyOtherCase -> Nothing

{-|
Remove one redundant save/restore pair in a single pass.

Detects two consecutive restore operators and removes the associated save
command that brackets them. This eliminates empty save/restore pairs.

The optimization works by:

* Finding two consecutive restore commands
* Locating the matching save command for the first restore
* Removing both the save and the first restore, leaving only the second restore

If the save/restore structure is unbalanced, this raises an error.
-}
reduceSaveRestoreOnePass :: Program -> Program
reduceSaveRestoreOnePass program = case findDoubleRestore program of
  -- Two consecutive restore commands means a save/restore pair is useless.
  Just (beforeDoubleRestore, afterDoubleRestore) ->
    -- Look for the save command associated with the restore command.
    case findRelatedSave beforeDoubleRestore of
      Just (beforeSave, Save :<| afterSave) ->
        beforeSave <> afterSave <> singleton Restore <> afterDoubleRestore
      _anythingElse -> error "unbalanced save/restore"
  _anyOtherCase -> program

{-|
Remove useless save/restore commands in a single pass.

Identifies and removes save/restore pairs that don't enclose any state-changing
operations. Specifically:

* If the next command after a restore is another restore or only marked content
  operators, the save/restore pair can be removed
* If a save/restore pair has no commands between them, both are removed

Recursively processes remaining commands after each removal. Uses
@nextRestoreWithoutNewState@ to check if the enclosed operations modify graphics
state.
-}
optimizeSaveRestoreOnePass :: Program -> Program
optimizeSaveRestoreOnePass program = case breakl onRestore program of
  -- Found a restore command.
  (beforeRestore, Restore :<| afterRestore) ->
    -- If the next commands do not change state, remove save/restore.
    if nextRestoreWithoutNewState afterRestore
    then
      case findRelatedSave beforeRestore of
        Just (beforeSave, Save :<| afterSave) ->
             beforeSave
          <> afterSave
          <> optimizeSaveRestoreOnePass afterRestore
        _anythingElse ->
             beforeRestore
          <> singleton Restore
          <> optimizeSaveRestoreOnePass afterRestore
    else
      -- Look for the save command associated with the restore command.
      case findRelatedSave beforeRestore of
        -- If there is no command between save and restore, remove both.
        Just (beforeSave, Save :<| Empty) ->
          beforeSave <> afterRestore
        _anythingElse ->
          beforeRestore <> (Restore <| optimizeSaveRestoreOnePass afterRestore)

  -- For any other case, just keep the program as is.
  _anyOtherCase -> program
 where
  nextRestoreWithoutNewState :: Program -> Bool
  nextRestoreWithoutNewState Empty = False
  nextRestoreWithoutNewState (command :<| rest) = case command of
    Command GSRestoreGS _params -> True
    Command GSBeginMarkedContentSequence _params ->
      nextRestoreWithoutNewState rest
    Command GSBeginMarkedContentSequencePL _params ->
      nextRestoreWithoutNewState rest
    Command GSEndMarkedContentSequence _params ->
      nextRestoreWithoutNewState rest
    _anyOtherCommand -> False

{-|
Check if a program has balanced save/restore and marked content operators.

Validates that all save (q), restore (Q), begin marked content (BMC/BDC), and
end marked content (EMC) operators are properly paired and nested.

Maintains a nesting level counter:

* Incremented by save and begin marked content operators
* Decremented by restore and end marked content operators
* Must equal zero at the end of the program

Returns @True@ if the program is balanced, @False@ otherwise.
-}
balancedProgram :: Int -> Program -> Bool
balancedProgram level Empty = level == 0
balancedProgram level (Command GSSaveGS _params :<| rest) =
  balancedProgram (level + 1) rest
balancedProgram level (Command GSBeginMarkedContentSequence _params :<| rest) =
  balancedProgram (level + 1) rest
balancedProgram level (Command GSRestoreGS _params :<| rest) =
  (level > 0) && balancedProgram (level - 1) rest
balancedProgram level (Command GSEndMarkedContentSequence _params :<| rest) =
  (level > 0) && balancedProgram (level - 1) rest
balancedProgram level (_anyOtherCommand :<| rest) = balancedProgram level rest

{-|
Optimize save/restore commands in a PDF graphics program.

Removes redundant and useless save/restore pairs, reducing the size of the
graphics stream while maintaining semantic correctness.

The optimization process:

* Validates that all save/restore operators are properly balanced
* Repeatedly removes pairs where two restores are consecutive (indicating empty
  save/restore blocks)
* Removes save/restore pairs that enclose only marked content operators

Returns the optimized program. If the input program has unbalanced save/restore
operators, returns it unchanged.
-}
optimizeSaveRestore :: Program -> Program
optimizeSaveRestore program =
  if balancedProgram 0 program
  then 
    let reduced = untilNoChange reduceSaveRestoreOnePass program
    in untilNoChange optimizeSaveRestoreOnePass reduced
  else program
