module Data.PDF.InterpreterAction
  ( InterpreterAction (ReplaceCommand, ReplaceAndDeleteNextCommand, DeleteCommand, KeepCommand)
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
  deriving stock (Eq, Show)

replaceCommandWith :: Command -> Command -> InterpreterAction
replaceCommandWith command newCommand
  | command == newCommand = KeepCommand
  | otherwise             = ReplaceCommand newCommand
