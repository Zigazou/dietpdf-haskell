module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeGeneric
  ( optimizeGeneric
  ) where

import Control.Monad.State (State)

import Data.Functor ((<&>))
import Data.PDF.Command (Command (cOperator))
import Data.PDF.InterpreterAction (InterpreterAction, replaceCommandWith)
import Data.PDF.InterpreterState
  (InterpreterState, usefulGraphicsPrecisionS, usefulTextPrecisionS)
import Data.PDF.OperatorCategory
  ( OperatorCategory (ClippingPathOperator, PathConstructionOperator, PathPaintingOperator, TextPositioningOperator, TextShowingOperator, TextStateOperator, Type3FontOperator)
  , category
  )
import Data.PDF.Program (Program)

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

  _anyOtherCategory -> replaceCommandWith command
                     . optimizeParameters command
                   <$> usefulGraphicsPrecisionS
