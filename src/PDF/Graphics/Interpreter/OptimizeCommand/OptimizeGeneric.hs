{-|
Generic optimization for graphics stream commands.

Provides category-based optimization that applies appropriate precision
reduction depending on command type (path, text, clipping, etc.).
-}
module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeGeneric
  ( optimizeGeneric
  ) where

import Control.Monad.State (State)

import Data.Functor ((<&>))
import Data.PDF.Command (Command (cOperator))
import Data.PDF.InterpreterAction
  (InterpreterAction (KeepCommand), replaceCommandWith)
import Data.PDF.InterpreterState
  (InterpreterState, usefulGraphicsPrecisionS, usefulTextPrecisionS)
import Data.PDF.OperatorCategory
  ( OperatorCategory (ClippingPathOperator, ColorOperator, PathConstructionOperator, PathPaintingOperator, SpecialGraphicsStateOperator, TextPositioningOperator, TextShowingOperator, TextStateOperator, Type3FontOperator)
  , category
  )
import Data.PDF.Program (Program)

import PDF.Graphics.Interpreter.OptimizeParameters (optimizeParameters)

{-|
Optimize a graphics command based on its category.

Applies precision reduction appropriate to the command's operator category:

* __Graphics State__: No optimization (kept as-is)
* __Path Construction, Painting, Clipping__: Reduced graphics precision
* __Text State, Type3 Font, Text Positioning, Text Showing__: Reduced text
  precision
* __Color Operations__: No optimization (handled separately)
* __Other__: Reduced graphics precision

Precision values are obtained from interpreter settings and applied via
'optimizeParameters'.
-}
optimizeGeneric
  :: Command
  -> Program
  -> State InterpreterState InterpreterAction
optimizeGeneric command _rest = case category (cOperator command) of
  SpecialGraphicsStateOperator -> return KeepCommand

  PathConstructionOperator ->
    optimizeParameters command
      <$> usefulGraphicsPrecisionS
      <&> replaceCommandWith command

  PathPaintingOperator ->
    optimizeParameters command
      <$> usefulGraphicsPrecisionS
      <&> replaceCommandWith command

  ClippingPathOperator ->
    optimizeParameters command
      <$> usefulGraphicsPrecisionS
      <&> replaceCommandWith command

  TextStateOperator ->
    optimizeParameters command
      <$> usefulTextPrecisionS
      <&> replaceCommandWith command

  Type3FontOperator ->
    optimizeParameters command
      <$> usefulTextPrecisionS
      <&> replaceCommandWith command

  TextPositioningOperator ->
    optimizeParameters command
      <$> usefulTextPrecisionS
      <&> replaceCommandWith command

  TextShowingOperator ->
    optimizeParameters command
      <$> usefulTextPrecisionS
      <&> replaceCommandWith command

  ColorOperator -> return KeepCommand

  _anyOtherCategory -> replaceCommandWith command
                     . optimizeParameters command
                   <$> usefulGraphicsPrecisionS
