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

onRestore :: Command -> Bool
onRestore (Command GSRestoreGS _params) = True
onRestore _anyOtherCommand              = False

pattern Restore :: Command
pattern Restore = Command GSRestoreGS Empty

pattern Save :: Command
pattern Save = Command GSSaveGS Empty

{- |
Find the save command associated with a restore command.

This function starts from the end of the program and looks for the save command
associated with the restore command. The restore command must not be included in
the program given in input.
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

findDoubleRestore :: Program -> Maybe (Program, Program)
findDoubleRestore program = case breakl onRestore program of
  (beforeRestore, Restore :<| Restore :<| afterRestore) ->
    Just (beforeRestore, afterRestore)
  (beforeRestore, Restore :<| afterRestore) -> do
    (beforeRestore', afterRestore') <- findDoubleRestore afterRestore
    Just (beforeRestore <> singleton Restore <> beforeRestore', afterRestore')
  _anyOtherCase -> Nothing

{- |
Save one save/restore pair just before a restore operator in one pass.
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

{- |
Remove useless save/restore commands.
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

{- |
Check if a program has balanced save/restore commands.
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

optimizeSaveRestore :: Program -> Program
optimizeSaveRestore program =
  if balancedProgram 0 program
  then 
    let reduced = untilNoChange reduceSaveRestoreOnePass program
    in untilNoChange optimizeSaveRestoreOnePass reduced
  else program
