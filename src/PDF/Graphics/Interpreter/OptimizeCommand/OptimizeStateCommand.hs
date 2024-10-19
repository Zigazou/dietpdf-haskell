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

deleteIfNothingWillChange
  :: Command
  -> Double
  -> (GraphicsState -> Double)
  -> (Double -> State InterpreterState ())
  -> State InterpreterState InterpreterAction
deleteIfNothingWillChange command newValue getter setter = do
    newValue' <- usefulGraphicsPrecisionS <&> flip round' newValue
    currentValue <- gets (getter . iGraphicsState)
    if newValue' == currentValue
      then return DeleteCommand
      else do
        setter newValue'
        ReplaceCommand . optimizeParameters command <$> usefulGraphicsPrecisionS

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

  (GSSetLineWidth, GFXNumber width :<| Empty) ->
    deleteIfNothingWillChange command width gsLineWidth setLineWidthS

  (GSSetLineCap, GFXNumber lineCap :<| Empty) ->
    deleteIfNothingWillChange command lineCap gsLineCap setLineCapS

  (GSSetLineJoin, GFXNumber lineJoin :<| Empty) ->
    deleteIfNothingWillChange command lineJoin gsLineJoin setLineJoinS

  (GSSetMiterLimit, GFXNumber miterLimit :<| Empty) ->
    deleteIfNothingWillChange command miterLimit gsMiterLimit setMiterLimitS

  (GSSetFlatnessTolerance, GFXNumber flatness :<| Empty) ->
    deleteIfNothingWillChange command flatness gsFlatness setFlatnessS

  _anyOtherCommand -> return KeepCommand
 where
  operator   = cOperator command
  parameters = cParameters command
