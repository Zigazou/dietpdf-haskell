module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeDrawCommand
  ( optimizeDrawCommand
  ) where

import Control.Monad.State (State, gets)

import Data.Functor ((<&>))
import Data.PDF.Command (Command (Command, cOperator, cParameters))
import Data.PDF.GFXObject
    ( GFXObject (GFXNumber)
    , GSOperator (GSCloseSubpath, GSCubicBezierCurve, GSCubicBezierCurve1To, GSCubicBezierCurve2To, GSEndPath, GSLineTo, GSMoveTo)
    )
import Data.PDF.GraphicsState
    ( GraphicsState (gsCurrentPointX, gsCurrentPointY, gsPathStartX, gsPathStartY)
    )
import Data.PDF.InterpreterAction
    ( InterpreterAction (DeleteCommand, KeepCommand, ReplaceCommand)
    )
import Data.PDF.InterpreterState
    ( InterpreterState (iGraphicsState)
    , getPathStartS
    , setCurrentPointS
    , setPathStartS
    , usefulGraphicsPrecisionS
    )
import Data.PDF.Program (Program)
import Data.Sequence (Seq (Empty, (:<|)))

import PDF.Graphics.Interpreter.OptimizeParameters (optimizeParameters)

import Util.Graphics (areAligned)
import Util.Number (round')


{- |
The 'optimizeDrawCommand' function takes a 'GraphicsState' and a 'Command' and
returns an optimized 'Command'.
-}
optimizeDrawCommand
  :: Command
  -> Program
  -> State InterpreterState InterpreterAction
optimizeDrawCommand command rest = case (operator, parameters) of
  -- MoveTo operator
  (GSMoveTo, GFXNumber x :<| GFXNumber y :<| Empty) -> do
    setPathStartS x y
    ReplaceCommand . optimizeParameters command <$> usefulGraphicsPrecisionS

  -- Keep track of current position
  (GSCubicBezierCurve, GFXNumber x1 :<| GFXNumber y1
                   :<| GFXNumber x2 :<| GFXNumber y2
                   :<| GFXNumber x3 :<| GFXNumber y3
                   :<| Empty) -> do
    precision <- usefulGraphicsPrecisionS
    let x1' = round' (precision - 1) x1
        y1' = round' (precision - 1) y1
        x2' = round' (precision - 1) x2
        y2' = round' (precision - 1) y2
        x3' = round' precision x3
        y3' = round' precision y3
    setCurrentPointS x3' y3'
    return
      (ReplaceCommand
        (Command GSCubicBezierCurve (GFXNumber x1' :<| GFXNumber y1'
                                 :<| GFXNumber x2' :<| GFXNumber y2'
                                 :<| GFXNumber x3' :<| GFXNumber y3'
                                 :<| Empty)
        )
      )

  (GSCubicBezierCurve1To, GFXNumber x1 :<| GFXNumber y1
                      :<| GFXNumber x3 :<| GFXNumber y3
                      :<| Empty) -> do
    precision <- usefulGraphicsPrecisionS
    let x1' = round' (precision - 1) x1
        y1' = round' (precision - 1) y1
        x3' = round' precision x3
        y3' = round' precision y3
    setCurrentPointS x3' y3'
    return
      (ReplaceCommand
        (Command GSCubicBezierCurve1To (GFXNumber x1' :<| GFXNumber y1'
                                    :<| GFXNumber x3' :<| GFXNumber y3'
                                    :<| Empty)
        )
      )

  (GSCubicBezierCurve2To, GFXNumber x2 :<| GFXNumber y2
                      :<| GFXNumber x3 :<| GFXNumber y3
                      :<| Empty) -> do
    precision <- usefulGraphicsPrecisionS
    let x2' = round' (precision - 1) x2
        y2' = round' (precision - 1) y2
        x3' = round' precision x3
        y3' = round' precision y3
    setCurrentPointS x3' y3'
    return
      (ReplaceCommand
        (Command GSCubicBezierCurve2To (GFXNumber x2' :<| GFXNumber y2'
                                    :<| GFXNumber x3' :<| GFXNumber y3'
                                    :<| Empty)
        )
      )

  -- Optimize LineTo operator
  (GSLineTo, GFXNumber x :<| GFXNumber y :<| Empty) -> do
    currentX <- gets (gsCurrentPointX . iGraphicsState)
    currentY <- gets (gsCurrentPointY . iGraphicsState)
    startX   <- gets (gsPathStartX . iGraphicsState)
    startY   <- gets (gsPathStartY . iGraphicsState)

    -- Calculate next coordinates.
    let
      (lineIsNotNeeded, newX, newY) = case rest of
        (Command GSEndPath _params :<| _tail) ->
          ( areAligned (currentX, currentY) (x, y) (startX, startY) 0, x, y )
        (Command GSCloseSubpath _params :<| _tail) ->
          ( areAligned (currentX, currentY) (x, y) (startX, startY) 0, x, y )
        (Command GSLineTo (GFXNumber nextX :<| GFXNumber nextY :<| Empty)
                      :<| _tail) ->
          ( areAligned (currentX, currentY) (x, y) (nextX, nextY) 0
          , nextX
          , nextY
          )
        _otherCommand -> (False, x, y)

    setCurrentPointS newX newY

    -- If the LineTo operator has coordinates on the line between the current
    -- point and the next coordinates, then remove the LineTo operator.
    if lineIsNotNeeded
      then return DeleteCommand
      else do
        optimized <- usefulGraphicsPrecisionS <&> optimizeParameters command

        -- If the LineTo operator is followed by a CloseSubpath operator and it
        -- goes back to the start of the path, then remove the LineTo operator
        -- because the CloseSubpath operator will close the path.
        start <- getPathStartS
        return $ case rest of
          (Command GSEndPath _params :<| _tail) ->
            if (x, y) == start
              then ReplaceCommand (Command GSCloseSubpath mempty)
              else ReplaceCommand optimized
          (Command GSCloseSubpath _params :<| _tail) ->
            if (x, y) == start
              then DeleteCommand
              else ReplaceCommand optimized
          _otherCommand -> ReplaceCommand optimized

  -- EndPath operator
  (GSEndPath, _params) -> do
    setPathStartS 0 0
    return KeepCommand

  -- CloseSubpath operator
  (GSCloseSubpath, _params) -> do
    setPathStartS 0 0
    return KeepCommand

  _anyOtherCommand -> return KeepCommand
 where
  operator   = cOperator command
  parameters = cParameters command
