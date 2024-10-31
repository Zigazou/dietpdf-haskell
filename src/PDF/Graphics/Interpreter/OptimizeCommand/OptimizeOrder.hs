module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeOrder
  ( optimizeOrder
  ) where

import Control.Monad.State (State)

import Data.PDF.Command (Command (cOperator))
import Data.PDF.GFXObject
  ( GSOperator (GSSetBoundingBoxGlyph, GSSetCharacterSpacing, GSSetColourRenderingIntent, GSSetFlatnessTolerance, GSSetGlyphWidth, GSSetHorizontalScaling, GSSetLineCap, GSSetLineDashPattern, GSSetLineJoin, GSSetLineWidth, GSSetMiterLimit, GSSetNonStrokeCMYKColorspace, GSSetNonStrokeColor, GSSetNonStrokeColorN, GSSetNonStrokeColorspace, GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetStrokeCMYKColorspace, GSSetStrokeCMYKColorspace, GSSetStrokeColor, GSSetStrokeColorN, GSSetStrokeColorspace, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace, GSSetTextFont, GSSetTextLeading, GSSetTextMatrix, GSSetTextRenderingMode, GSSetTextRise, GSSetWordSpacing)
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

operatorWeight :: GSOperator -> Maybe Int
operatorWeight GSSetStrokeColorspace        = Just 1
operatorWeight GSSetStrokeColor             = Just 1
operatorWeight GSSetStrokeColorN            = Just 1
operatorWeight GSSetStrokeGrayColorspace    = Just 1
operatorWeight GSSetStrokeRGBColorspace     = Just 1
operatorWeight GSSetStrokeCMYKColorspace    = Just 1
operatorWeight GSSetNonStrokeCMYKColorspace = Just 2
operatorWeight GSSetNonStrokeColorspace     = Just 2
operatorWeight GSSetNonStrokeColor          = Just 2
operatorWeight GSSetNonStrokeColorN         = Just 2
operatorWeight GSSetNonStrokeGrayColorspace = Just 2
operatorWeight GSSetNonStrokeRGBColorspace  = Just 2
operatorWeight GSSetLineWidth               = Just 10
operatorWeight GSSetLineCap                 = Just 11
operatorWeight GSSetLineJoin                = Just 12
operatorWeight GSSetMiterLimit              = Just 13
operatorWeight GSSetLineDashPattern         = Just 14
operatorWeight GSSetColourRenderingIntent   = Just 15
operatorWeight GSSetFlatnessTolerance       = Just 16
operatorWeight GSSetTextMatrix              = Just 17
operatorWeight GSSetCharacterSpacing        = Just 18
operatorWeight GSSetWordSpacing             = Just 19
operatorWeight GSSetHorizontalScaling       = Just 20
operatorWeight GSSetTextLeading             = Just 21
operatorWeight GSSetTextFont                = Just 22
operatorWeight GSSetTextRenderingMode       = Just 23
operatorWeight GSSetTextRise                = Just 24
operatorWeight GSSetGlyphWidth              = Just 25
operatorWeight GSSetBoundingBoxGlyph        = Just 26
operatorWeight _anyOtherOperator            = Nothing

optimizeOrder
  :: Command
  -> Program
  -> State InterpreterState InterpreterAction
optimizeOrder command (nextCommand :<| _rest) =
  case (category operator, category nextOperator) of
    (TextStateOperator, SpecialGraphicsStateOperator) -> return SwitchCommand
    (ColorOperator, SpecialGraphicsStateOperator)     -> return SwitchCommand
    _anyOtherCombination -> case (operatorWeight operator, operatorWeight nextOperator) of
      (Nothing, _anyOtherWeight) -> return KeepCommand
      (_anyOtherWeight, Nothing) -> return KeepCommand
      (Just weight, Just nextWeight) -> if nextWeight < weight
        then return SwitchCommand
        else return KeepCommand
 where
  operator :: GSOperator
  operator = cOperator command

  nextOperator :: GSOperator
  nextOperator = cOperator nextCommand

optimizeOrder _command _rest = return KeepCommand
