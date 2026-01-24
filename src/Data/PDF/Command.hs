{-|
PDF graphics content stream commands.

In a PDF content stream, graphics operations are expressed as an operator (for
example, "m", "l", "cm", ...) followed by a list of operands.

This module models such an operation as `Command` and provides `mkCommand` as a
convenience constructor.
-}
module Data.PDF.Command
  ( Command (Command, cOperator, cParameters)
  , mkCommand
  )
where

import Data.Kind (Type)
import Data.PDF.GFXObject (GFXObject, GSOperator)
import Data.PDF.GFXObjects (GFXObjects)
import Data.Sequence (fromList)

{-|
`Command` represents a graphics-state operator together with its operands.
-}
type Command :: Type
data Command = Command
  { cOperator   :: !GSOperator -- ^ Graphics-state operator
  , cParameters :: !GFXObjects -- ^ Graphics-state operands
  }
  deriving stock (Eq, Show)

{-|
Creates a `Command` from an operator and a list of operands.

The operands are converted to `GFXObjects`.
-}
mkCommand :: GSOperator -> [GFXObject] -> Command
mkCommand = (. fromList) . Command
