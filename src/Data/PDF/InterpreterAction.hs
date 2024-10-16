module Data.PDF.InterpreterAction
  ( InterpreterAction (ReplaceCommand, ReplaceAndDeleteNextCommand, DeleteCommand, KeepCommand)
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
