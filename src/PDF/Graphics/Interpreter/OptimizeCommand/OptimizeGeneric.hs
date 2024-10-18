module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeGeneric
  ( optimizeGeneric
  ) where

import Control.Monad.State (State)

import Data.PDF.Command (Command (cOperator))
import Data.PDF.InterpreterAction
    ( InterpreterAction (KeepCommand, ReplaceCommand)
    )
import Data.PDF.InterpreterState
    ( InterpreterState
    , usefulColorPrecisionS
    , usefulGraphicsPrecisionS
    , usefulTextPrecisionS
    )
import Data.PDF.OperatorCategory
    ( OperatorCategory (ClippingPathOperator, ColorOperator, PathConstructionOperator, PathPaintingOperator, TextPositioningOperator, TextShowingOperator, TextStateOperator, Type3FontOperator)
    , category
    )
import Data.PDF.Program (Program)

import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeColor (optimizeColor)
import PDF.Graphics.Interpreter.OptimizeParameters (optimizeParameters)



{- |
The 'optimizeGeneric' function takes a 'GraphicsState' and a 'Command' and
returns an optimized 'Command'.
-}
optimizeGeneric
  :: Command
  -> Program
  -> State InterpreterState InterpreterAction
optimizeGeneric command _rest = case category (cOperator command) of
  PathConstructionOperator ->
    ReplaceCommand . optimizeParameters command <$> usefulGraphicsPrecisionS
  PathPaintingOperator ->
    ReplaceCommand . optimizeParameters command <$> usefulGraphicsPrecisionS
  ClippingPathOperator ->
    ReplaceCommand . optimizeParameters command <$> usefulGraphicsPrecisionS
  TextStateOperator ->
    ReplaceCommand . optimizeParameters command <$> usefulTextPrecisionS
  Type3FontOperator ->
    ReplaceCommand . optimizeParameters command <$> usefulTextPrecisionS
  TextPositioningOperator ->
    ReplaceCommand . optimizeParameters command <$> usefulTextPrecisionS
  TextShowingOperator ->
    ReplaceCommand . optimizeParameters command <$> usefulTextPrecisionS
  ColorOperator -> ReplaceCommand
                 . optimizeColor
                 . optimizeParameters command
               <$> usefulColorPrecisionS

  _anyOtherCategory -> return KeepCommand
