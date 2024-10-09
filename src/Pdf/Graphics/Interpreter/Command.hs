module Pdf.Graphics.Interpreter.Command
  ( Command (Command, cOperator, cParameters)
  , mkCommand
  )
where

import Data.Kind (Type)
import Data.Sequence (fromList)

import Pdf.Graphics.Object (GFXObject, GSOperator)
import Pdf.Graphics.Objects (Objects)

{- |
The 'Command' type represents a graphics state operator and its parameters.
-}
type Command :: Type
data Command = Command
  { cOperator   :: !GSOperator
  , cParameters :: !Objects
  }
  deriving stock (Eq, Show)

mkCommand :: GSOperator -> [GFXObject] -> Command
mkCommand = (. fromList) . Command
