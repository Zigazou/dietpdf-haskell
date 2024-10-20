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

{- |
Find the save command associated with a restore command.

This function starts from the end of the program and looks for the save command
associated with the restore command. The restore command must not be included in
the program given in input.
-}
findRelatedSave :: Program -> Maybe (Program, Program)
findRelatedSave program = findRelatedSave' 1 (Just (program, mempty))
 where
  findRelatedSave' :: Int -> Maybe (Program, Program) -> Maybe (Program, Program)
  findRelatedSave' 0 result = result
  findRelatedSave' level (Just (before :|> restore@(Command GSRestoreGS _params), after)) =
    findRelatedSave' (level + 1) (Just (before, restore <| after))
  findRelatedSave' level (Just (before :|> save@(Command GSSaveGS _params), after)) =
    findRelatedSave' (level - 1) (Just (before, save <| after))
  findRelatedSave' level (Just (before :|> command, after)) =
    findRelatedSave' level (Just (before, command <| after))
  findRelatedSave' level (Just (command :<| Empty, after)) =
    findRelatedSave' level (Just (mempty, command <| after))
  findRelatedSave' _level _ = Nothing

{- |
Remove useless save/restore commands.
-}
optimizeSaveRestoreOnePass :: Program -> Program
optimizeSaveRestoreOnePass program = case breakl onRestore program of
  -- Two consecutive restore commands means a save/restore pair is useless.
  (beforeRestore, Command GSRestoreGS _params1
              :<| Command GSRestoreGS _params2
              :<| afterRestore) ->
    -- Look for the save command associated with the restore command.
    case findRelatedSave beforeRestore of
      Just (beforeSave, Command GSSaveGS _params :<| afterSave) ->
           beforeSave
        <> afterSave
        <> singleton (Command GSRestoreGS mempty)
        <> optimizeSaveRestoreOnePass afterRestore
      _anythingElse ->
           beforeRestore
        <> singleton (Command GSRestoreGS mempty)
        <> singleton (Command GSRestoreGS mempty)
        <> optimizeSaveRestoreOnePass afterRestore

  -- Found a restore command not followed by another restore command.
  (beforeRestore, Command GSRestoreGS _params
              :<| afterRestore) ->
    if nextRestoreWithoutNewState afterRestore
    then
      case findRelatedSave beforeRestore of
        Just (beforeSave, Command GSSaveGS _params :<| afterSave) ->
            beforeSave
          <> afterSave
          <> optimizeSaveRestoreOnePass afterRestore
        _anythingElse ->
            beforeRestore
          <> singleton (Command GSRestoreGS mempty)
          <> singleton (Command GSRestoreGS mempty)
          <> optimizeSaveRestoreOnePass afterRestore
    else
      -- Look for the save command associated with the restore command.
      case findRelatedSave beforeRestore of
        -- If there is no command between save and restore, remove both.
        Just (beforeSave, Command GSSaveGS _params :<| Empty) ->
            beforeSave
          <> optimizeSaveRestoreOnePass afterRestore
        _anythingElse ->
            beforeRestore
          <> (  Command GSRestoreGS mempty
             <| optimizeSaveRestoreOnePass afterRestore
             )

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

optimizeSaveRestore :: Program -> Program
optimizeSaveRestore = untilNoChange optimizeSaveRestoreOnePass
