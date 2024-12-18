module Data.PDF.InterpreterAction
  ( InterpreterAction (ReplaceCommand, ReplaceAndDeleteNextCommand, DeleteCommand, KeepCommand, SwitchCommand)
  , replaceCommandWith
  )
where

import Data.Kind (Type)
import Data.PDF.Command (Command)

type InterpreterAction :: Type
data InterpreterAction
  = ReplaceCommand !Command
  | ReplaceAndDeleteNextCommand !Command
  | DeleteCommand
  | KeepCommand
  | SwitchCommand
  deriving stock (Eq, Show)

replaceCommandWith :: Command -> Command -> InterpreterAction
replaceCommandWith command newCommand
  | command == newCommand = KeepCommand
  | otherwise             = ReplaceCommand newCommand
