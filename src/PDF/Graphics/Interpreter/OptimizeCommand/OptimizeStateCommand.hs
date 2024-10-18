module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeStateCommand
  ( optimizeStateCommand
  ) where

import Control.Monad.State (State, gets)

import Data.Functor ((<&>))
import Data.PDF.Command (Command (cOperator, cParameters))
import Data.PDF.GFXObject
    ( GFXObject (GFXNumber)
    , GSOperator (GSRestoreGS, GSSaveGS, GSSetFlatnessTolerance, GSSetLineCap, GSSetLineJoin, GSSetLineWidth, GSSetMiterLimit)
    )
import Data.PDF.GraphicsState
    ( GraphicsState (gsFlatness, gsLineCap, gsLineJoin, gsLineWidth, gsMiterLimit)
    )
import Data.PDF.InterpreterAction
    ( InterpreterAction (DeleteCommand, KeepCommand, ReplaceCommand)
    )
import Data.PDF.InterpreterState
    ( InterpreterState (iGraphicsState)
    , restoreStateS
    , saveStateS
    , setFlatnessS
    , setLineCapS
    , setLineJoinS
    , setLineWidthS
    , setMiterLimitS
    , usefulGraphicsPrecisionS
    )
import Data.PDF.Program (Program)
import Data.Sequence (Seq (Empty, (:<|)))

import PDF.Graphics.Interpreter.OptimizeParameters (optimizeParameters)

import Util.Number (round')

{- |
The 'optimizeStateCommand' function takes a 'GraphicsState' and a 'Command' and
returns an optimized 'Command'.
-}
optimizeStateCommand
  :: Command
  -> Program
  -> State InterpreterState InterpreterAction
optimizeStateCommand command _rest = case (operator, parameters) of
  -- Save graphics state
  (GSSaveGS, _params) -> do
    saveStateS
    return KeepCommand

  -- Restore graphics state
  (GSRestoreGS, _params) -> do
    restoreStateS
    return KeepCommand

  (GSSetLineWidth, GFXNumber width :<| Empty) -> do
    width' <- usefulGraphicsPrecisionS <&> flip round' width
    currentWidth <- gets (gsLineWidth . iGraphicsState)
    if width' == currentWidth
      then return DeleteCommand
      else do
        setLineWidthS width'
        ReplaceCommand . optimizeParameters command <$> usefulGraphicsPrecisionS

  (GSSetLineCap, GFXNumber lineCap :<| Empty) -> do
    lineCap' <- usefulGraphicsPrecisionS <&> flip round' lineCap
    currentLineCap <- gets (gsLineCap . iGraphicsState)
    if lineCap' == currentLineCap
      then return DeleteCommand
      else do
        setLineCapS lineCap'
        ReplaceCommand . optimizeParameters command <$> usefulGraphicsPrecisionS

  (GSSetLineJoin, GFXNumber lineJoin :<| Empty) -> do
    lineJoin' <- usefulGraphicsPrecisionS <&> flip round' lineJoin
    currentLineJoin <- gets (gsLineJoin . iGraphicsState)
    if lineJoin' == currentLineJoin
      then return DeleteCommand
      else do
        setLineJoinS lineJoin'
        ReplaceCommand . optimizeParameters command <$> usefulGraphicsPrecisionS

  (GSSetMiterLimit, GFXNumber miterLimit :<| Empty) -> do
    miterLimit' <- usefulGraphicsPrecisionS <&> flip round' miterLimit
    currentMiterLimit <- gets (gsMiterLimit . iGraphicsState)
    if miterLimit' == currentMiterLimit
      then return DeleteCommand
      else do
        setMiterLimitS miterLimit'
        ReplaceCommand . optimizeParameters command <$> usefulGraphicsPrecisionS

  (GSSetFlatnessTolerance, GFXNumber flatness :<| Empty) -> do
    flatness' <- usefulGraphicsPrecisionS <&> flip round' flatness
    currentFlatness <- gets (gsFlatness . iGraphicsState)
    if flatness' == currentFlatness
      then return DeleteCommand
      else do
        setFlatnessS flatness'
        ReplaceCommand . optimizeParameters command <$> usefulGraphicsPrecisionS

  _anyOtherCommand -> return KeepCommand
 where
  operator   = cOperator command
  parameters = cParameters command
