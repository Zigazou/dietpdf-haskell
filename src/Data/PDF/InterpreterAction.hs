{-|
Actions produced by command-rewriting passes.

This module defines the small set of actions a rewriting/interpreting pass can
return when looking at a PDF content-stream 'Command'.

The intent is to make transformation passes explicit: rather than directly
mutating a list of commands, a pass can decide to keep, delete, replace, or
signal a control-flow change.
-}
module Data.PDF.InterpreterAction
  ( InterpreterAction (ReplaceCommand, ReplaceAndDeleteNextCommand, DeleteCommand, KeepCommand, SwitchCommand)
  , replaceCommandWith
  )
where

import Data.Kind (Type)
import Data.PDF.Command (Command)

{-|
Result of interpreting/rewriting a single 'Command'.

Each constructor describes how the caller should handle the current command (and
in one case, also the next command).
-}
type InterpreterAction :: Type
data InterpreterAction
  {-|
  Replace the current command with the given one.
  -}
  = ReplaceCommand !Command
  {-|
  Replace the current command with the given one, and delete the next command.
  -}
  | ReplaceAndDeleteNextCommand !Command
  {-|
  Delete the current command.
  -}
  | DeleteCommand
  {-|
  Keep the current command unchanged.
  -}
  | KeepCommand
  {-|
  Signal that the interpreter should switch two consecutive commands.
  -}
  | SwitchCommand
  deriving stock (Eq, Show)

{-|
Convenience helper: replace only when needed.

If the new command is identical to the original, this returns 'KeepCommand'
instead of 'ReplaceCommand'.
-}
replaceCommandWith :: Command -> Command -> InterpreterAction
replaceCommandWith command newCommand
  | command == newCommand = KeepCommand
  | otherwise             = ReplaceCommand newCommand
