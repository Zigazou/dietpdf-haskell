{-|
Optimize graphics state parameter commands.

Provides utilities for eliminating redundant graphics state settings by tracking
state values and detecting unchanged parameters (line width, line cap, line
join, miter limit, flatness).
-}
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
  (GraphicsState (gsFlatness, gsLineCap, gsLineJoin, gsLineWidth, gsMiterLimit))
import Data.PDF.InterpreterAction
  (InterpreterAction (DeleteCommand, KeepCommand), replaceCommandWith)
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

{-|
Delete or optimize a graphics state command if its value hasn't changed.

Compares the new value (after precision reduction) with the current graphics
state value. If they match, deletes the command as redundant. Otherwise, updates
the state and returns an optimized version of the command with reduced
precision.

Used for state parameters like line width, line cap, line join, etc.
-}
deleteIfNoChange
  :: Command
  -> Double
  -> (GraphicsState -> Double)
  -> (Double -> State InterpreterState ())
  -> State InterpreterState InterpreterAction
deleteIfNoChange command newValue getter setter = do
    newValue' <- usefulGraphicsPrecisionS <&> flip round' newValue
    currentValue <- gets (getter . iGraphicsState)
    if newValue' == currentValue
      then return DeleteCommand
      else do
        setter newValue'
        optimizeParameters command
          <$> usefulGraphicsPrecisionS
          <&> replaceCommandWith command

{-|
Optimize a graphics state parameter command.

Optimizes commands that modify graphics state parameters:

* __Save/Restore__: Tracked but kept (save/restore state stack)
* __Line Width, Cap, Join, Miter Limit, Flatness__: Deleted if the value hasn't
  changed since last setting; otherwise optimized with reduced precision and the
  state is updated

Returns 'KeepCommand' for save/restore, 'DeleteCommand' if no change, or
optimized command with updated state for parameter changes.
-}
optimizeStateCommand
  :: Command
  -> Program
  -> State InterpreterState InterpreterAction
optimizeStateCommand command _rest = case (operator, parameters) of
  -- Save graphics state
  (GSSaveGS, Empty) -> saveStateS >> return KeepCommand

  -- Restore graphics state
  (GSRestoreGS, Empty) -> restoreStateS >> return KeepCommand

  (GSSetLineWidth, GFXNumber width :<| Empty) ->
    deleteIfNoChange command width gsLineWidth setLineWidthS

  (GSSetLineCap, GFXNumber lineCap :<| Empty) ->
    deleteIfNoChange command lineCap gsLineCap setLineCapS

  (GSSetLineJoin, GFXNumber lineJoin :<| Empty) ->
    deleteIfNoChange command lineJoin gsLineJoin setLineJoinS

  (GSSetMiterLimit, GFXNumber miterLimit :<| Empty) ->
    deleteIfNoChange command miterLimit gsMiterLimit setMiterLimitS

  (GSSetFlatnessTolerance, GFXNumber flatness :<| Empty) ->
    deleteIfNoChange command flatness gsFlatness setFlatnessS

  _anyOtherCommand -> return KeepCommand
 where
  operator   = cOperator command
  parameters = cParameters command
