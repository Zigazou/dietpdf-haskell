module Pdf.Graphics.Interpreter.Command
  ( Command (Command, cOperator, cParameters)
  , mkCommand
  )
where

import Data.Kind (Type)
import Data.PDF.GFXObject (GFXObject, GSOperator)
import Data.PDF.GFXObjects (GFXObjects)
import Data.Sequence (fromList)

{- |
The 'Command' type represents a graphics state operator and its parameters.
-}
type Command :: Type
data Command = Command
  { cOperator   :: !GSOperator
  , cParameters :: !GFXObjects
  }
  deriving stock (Eq, Show)

mkCommand :: GSOperator -> [GFXObject] -> Command
mkCommand = (. fromList) . Command
