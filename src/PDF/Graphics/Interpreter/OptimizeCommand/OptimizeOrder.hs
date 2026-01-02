{-|
Optimize the order of graphics commands.

Provides utilities for reordering graphics state commands to improve efficiency
by executing independent operations in a canonical order that maximizes
opportunities for subsequent optimization passes.
-}
module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeOrder
  ( optimizeOrder
  ) where

import Control.Monad.State (State)

import Data.Kind (Type)
import Data.PDF.Command (Command (cOperator))
import Data.PDF.GFXObject
  ( GSOperator (GSSetBoundingBoxGlyph, GSSetCharacterSpacing, GSSetColourRenderingIntent, GSSetFlatnessTolerance, GSSetGlyphWidth, GSSetHorizontalScaling, GSSetLineCap, GSSetLineDashPattern, GSSetLineJoin, GSSetLineWidth, GSSetMiterLimit, GSSetNonStrokeCMYKColorspace, GSSetNonStrokeColor, GSSetNonStrokeColorN, GSSetNonStrokeColorspace, GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetStrokeCMYKColorspace, GSSetStrokeCMYKColorspace, GSSetStrokeColor, GSSetStrokeColorN, GSSetStrokeColorspace, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace, GSSetTextFont, GSSetTextLeading, GSSetTextMatrix, GSSetTextRenderingMode, GSSetTextRise, GSSetWordSpacing, GSSetCTM)
  )
import Data.PDF.InterpreterAction
  (InterpreterAction (KeepCommand, SwitchCommand))
import Data.PDF.InterpreterState (InterpreterState)
import Data.PDF.OperatorCategory
  ( OperatorCategory (ColorOperator, SpecialGraphicsStateOperator, TextStateOperator)
  , category
  )
import Data.PDF.Program (Program)
import Data.Sequence (Seq ((:<|)))

{-|
Weight value for operator ordering.

Operators are assigned weights that determine their preferred execution order.
Lower weights should be executed before higher weights. 'NoWeight' operators are
not reordered.
-}
type OperatorWeight :: Type
data OperatorWeight = Weight Int
                    | NoWeight
                    deriving stock (Eq, Ord)

{-|
Assign an execution weight to a graphics operator.

Weights determine the canonical ordering of operators. Lower weights execute
first. Operators with the same functional category may share weights. Operators
with 'NoWeight' are not reordered relative to other commands.

Weighting priority (lowest to highest):
1. Transformation matrices (CTM, TextMatrix)
2. Stroke colors
3. Line style parameters
4. Text state parameters
5. Non-stroke colors
-}
operatorWeight :: GSOperator -> OperatorWeight
operatorWeight GSSetCTM                     = Weight 0
operatorWeight GSSetTextMatrix              = Weight 1

operatorWeight GSSetStrokeColorspace        = Weight 3
operatorWeight GSSetStrokeColor             = Weight 3
operatorWeight GSSetStrokeColorN            = Weight 3
operatorWeight GSSetStrokeGrayColorspace    = Weight 3
operatorWeight GSSetStrokeRGBColorspace     = Weight 3
operatorWeight GSSetStrokeCMYKColorspace    = Weight 3
operatorWeight GSSetNonStrokeCMYKColorspace = Weight 6
operatorWeight GSSetNonStrokeColorspace     = Weight 6
operatorWeight GSSetNonStrokeColor          = Weight 6
operatorWeight GSSetNonStrokeColorN         = Weight 6
operatorWeight GSSetNonStrokeGrayColorspace = Weight 6
operatorWeight GSSetNonStrokeRGBColorspace  = Weight 6

operatorWeight GSSetLineWidth               = Weight 10
operatorWeight GSSetLineCap                 = Weight 11
operatorWeight GSSetLineJoin                = Weight 12
operatorWeight GSSetMiterLimit              = Weight 13
operatorWeight GSSetLineDashPattern         = Weight 14
operatorWeight GSSetColourRenderingIntent   = Weight 15
operatorWeight GSSetFlatnessTolerance       = Weight 16
operatorWeight GSSetTextFont                = Weight 17
operatorWeight GSSetCharacterSpacing        = Weight 18
operatorWeight GSSetWordSpacing             = Weight 19
operatorWeight GSSetHorizontalScaling       = Weight 20
operatorWeight GSSetTextLeading             = Weight 21
operatorWeight GSSetTextRenderingMode       = Weight 23
operatorWeight GSSetTextRise                = Weight 24
operatorWeight GSSetGlyphWidth              = Weight 25
operatorWeight GSSetBoundingBoxGlyph        = Weight 26
operatorWeight _anyOtherOperator            = NoWeight

{-|
Optimize the order of graphics commands.

Compares the current command with the next command and determines if they should
be swapped. Swapping occurs when:

* Text or Color state operators appear before transformation commands
* Operators are assigned weights and the next operator has lower weight (should
  execute first)

Returns 'SwitchCommand' to swap the current and next commands, or 'KeepCommand'
to maintain their order.
-}
optimizeOrder
  :: Command
  -> Program
  -> State InterpreterState InterpreterAction
optimizeOrder command (nextCommand :<| _rest) =
  case (category operator, category nextOperator) of
    (TextStateOperator, SpecialGraphicsStateOperator) -> return SwitchCommand
    (ColorOperator, SpecialGraphicsStateOperator)     -> return SwitchCommand
    _anyOtherCombination -> case (operatorWeight operator, operatorWeight nextOperator) of
      (NoWeight, _anyOtherWeight) -> return KeepCommand
      (_anyOtherWeight, NoWeight) -> return KeepCommand
      (weight, nextWeight) -> if nextWeight < weight
        then return SwitchCommand
        else return KeepCommand
 where
  operator :: GSOperator
  operator = cOperator command

  nextOperator :: GSOperator
  nextOperator = cOperator nextCommand

optimizeOrder _command _rest = return KeepCommand
