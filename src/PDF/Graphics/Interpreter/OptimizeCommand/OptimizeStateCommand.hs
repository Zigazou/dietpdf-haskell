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

import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.List (delete)
import Data.PDF.Command (Command (Command, cOperator, cParameters))
import Data.PDF.GFXObject
  ( GFXObject (GFXArray, GFXNumber)
  , GSOperator (GSRestoreGS, GSSaveGS, GSSetFlatnessTolerance, GSSetLineCap, GSSetLineDashPattern, GSSetLineJoin, GSSetLineWidth, GSSetMiterLimit, GSSetParameters)
  )
import Data.PDF.GraphicsState
  ( GraphicsState (gsDashArray, gsDashPhase, gsFlatness, gsLineCap, gsLineJoin, gsLineWidth, gsMiterLimit, gsUnknownParameters)
  , invalidateParameters
  , setDashPattern
  )
import Data.PDF.InterpreterAction
  ( InterpreterAction (DeleteCommand, KeepCommand, ReplaceCommand)
  , replaceCommandWith
  )
import Data.PDF.InterpreterState
  ( InterpreterState (iGraphicsState)
  , modifyGraphicsStateS
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
    unknown <- gets ( elem (cOperator command)
                    . gsUnknownParameters
                    . iGraphicsState
                    )
    if not unknown && newValue' == currentValue
      then return DeleteCommand
      else do
        setter newValue'
        markKnown (cOperator command)
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
  (GSSetParameters, _) ->
    modifyGraphicsStateS invalidateParameters >> return KeepCommand

  (GSSetLineDashPattern, GFXArray values :<| GFXNumber phase :<| Empty)
    | Just numbers <- traverse asNumber (toList values) -> do
        state <- gets iGraphicsState
        -- Preserve dash numbers exactly: rounding can turn a valid pattern
        -- into an invalid all-zero pattern.
        if notElem operator (gsUnknownParameters state)
           && numbers == gsDashArray state
           && phase == gsDashPhase state
          then
            return DeleteCommand
          else do
            modifyGraphicsStateS (setDashPattern numbers phase)
            markKnown operator
            return $ ReplaceCommand (Command operator parameters)

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

-- | Record newly known values in the shared graphics state.
markKnown :: GSOperator -> State InterpreterState ()
markKnown operator = modifyGraphicsStateS $ \state -> state
  { gsUnknownParameters = delete operator (gsUnknownParameters state) }

asNumber :: GFXObject -> Maybe Double
asNumber (GFXNumber value) = Just value
asNumber _other            = Nothing
