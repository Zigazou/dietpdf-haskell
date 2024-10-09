module Pdf.Graphics.Interpreter.OptimizeProgram
  ( optimizeProgram
  , findRelatedSave
  )
where

import Control.Monad.State (State, evalState)

import Data.FoldableWithRest (FoldableWithRest (foldMWithRest))
import Data.Functor ((<&>))
import Data.Sequence (Seq (Empty, (:<|), (:|>)), breakl, singleton, (<|), (|>))

import Pdf.Graphics.Interpreter.Command (Command (Command))
import Pdf.Graphics.Interpreter.GraphicsState
    ( GraphicsState
    , defaultGraphicsState
    )
import Pdf.Graphics.Interpreter.OptimizeCommand (optimizeCommand)
import Pdf.Graphics.Interpreter.Program (Program)
import Pdf.Graphics.Object (GSOperator (GSRestoreGS, GSSaveGS))

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
removeUselessSaveRestore :: Program -> Program
removeUselessSaveRestore program = case breakl onRestore program of
  -- Two consecutive restore commands means a save/restore pair is useless.
  (beforeRestore, Command GSRestoreGS _params1 :<| Command GSRestoreGS _params2 :<| afterRestore) ->
    -- Look for the save command associated with the restore command.
    case findRelatedSave beforeRestore of
      Just (beforeSave, Command GSSaveGS _params :<| afterSave) ->
           beforeSave
        <> afterSave
        <> singleton (Command GSRestoreGS mempty)
        <> removeUselessSaveRestore afterRestore
      _anythingElse ->
           beforeRestore
        <> singleton (Command GSRestoreGS mempty)
        <> singleton (Command GSRestoreGS mempty)
        <> removeUselessSaveRestore afterRestore

  -- Found a restore command not followed by another restore command.
  (beforeRestore, Command GSRestoreGS _params :<| afterRestore) ->
       beforeRestore
    <> (Command GSRestoreGS mempty <| removeUselessSaveRestore afterRestore)

  -- For any other case, just keep the program as is.
  _anyOtherCase -> program

{- |
The 'optimizeProgram' function takes a 'Program' and returns an optimized
'Program'.
-}
optimizeProgram :: Program -> Program
optimizeProgram = flip evalState defaultGraphicsState
                . foldMWithRest go mempty
                . removeUselessSaveRestore
  where
    go :: Program -> Command -> Program -> State GraphicsState Program
    go program command rest = optimizeCommand command rest <&> (program |>)
