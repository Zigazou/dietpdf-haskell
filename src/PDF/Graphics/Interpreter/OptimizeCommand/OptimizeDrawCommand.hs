module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeDrawCommand
  ( optimizeDrawCommand
  ) where

import Control.Monad.State (State, gets)

import Data.Functor ((<&>))
import Data.PDF.Command (Command (Command, cOperator, cParameters))
import Data.PDF.GFXObject
  ( GFXObject (GFXNumber)
  , GSOperator (GSCloseFillStrokeEOR, GSCloseFillStrokeNZWR, GSCloseStrokePath, GSCloseSubpath, GSCubicBezierCurve, GSCubicBezierCurve1To, GSCubicBezierCurve2To, GSEndPath, GSFillStrokePathEOR, GSFillStrokePathNZWR, GSLineTo, GSMoveTo, GSStrokePath)
  )
import Data.PDF.GraphicsState
  (GraphicsState (gsCurrentPointX, gsCurrentPointY, gsPathStartX, gsPathStartY))
import Data.PDF.InterpreterAction
  ( InterpreterAction (DeleteCommand, KeepCommand, ReplaceAndDeleteNextCommand)
  , replaceCommandWith
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
    optimizeParameters command <$> usefulGraphicsPrecisionS
      <&> replaceCommandWith command

  -- Keep track of current position
  (GSCubicBezierCurve, GFXNumber x1 :<| GFXNumber y1
                   :<| GFXNumber x2 :<| GFXNumber y2
                   :<| GFXNumber x3 :<| GFXNumber y3
                   :<| Empty) -> do
    precision <- usefulGraphicsPrecisionS
    let x1' = round' precision x1
        y1' = round' precision y1
        x2' = round' precision x2
        y2' = round' precision y2
        x3' = round' precision x3
        y3' = round' precision y3

    currentX <- gets (gsCurrentPointX . iGraphicsState)
    currentY <- gets (gsCurrentPointY . iGraphicsState)
    if | (x1', y1') == (currentX, currentY) -> do
          setCurrentPointS x3' y3'
          let optimizedCommand =
                Command GSCubicBezierCurve1To
                  (   GFXNumber x2' :<| GFXNumber y2'
                  :<| GFXNumber x3' :<| GFXNumber y3'
                  :<| Empty
                  )

          return $ replaceCommandWith command optimizedCommand

       | (x2', y2') == (currentX, currentY) -> do
          setCurrentPointS x3' y3'
          let optimizedCommand =
                Command GSCubicBezierCurve2To
                  (   GFXNumber x1' :<| GFXNumber y1'
                  :<| GFXNumber x3' :<| GFXNumber y3'
                  :<| Empty
                  )

          return $ replaceCommandWith command optimizedCommand

       | otherwise -> do
          setCurrentPointS x3' y3'

          let optimizedCommand =
                Command GSCubicBezierCurve
                  (   GFXNumber x1' :<| GFXNumber y1'
                  :<| GFXNumber x2' :<| GFXNumber y2'
                  :<| GFXNumber x3' :<| GFXNumber y3'
                  :<| Empty
                  )

          return $ replaceCommandWith command optimizedCommand

  (GSCubicBezierCurve1To, GFXNumber x1 :<| GFXNumber y1
                      :<| GFXNumber x3 :<| GFXNumber y3
                      :<| Empty) -> do
    precision <- usefulGraphicsPrecisionS
    let x1' = round' precision x1
        y1' = round' precision y1
        x3' = round' precision x3
        y3' = round' precision y3
    setCurrentPointS x3' y3'

    let optimizedCommand =
          Command GSCubicBezierCurve1To
            (   GFXNumber x1' :<| GFXNumber y1'
            :<| GFXNumber x3' :<| GFXNumber y3'
            :<| Empty
            )

    return $ replaceCommandWith command optimizedCommand

  (GSCubicBezierCurve2To, GFXNumber x2 :<| GFXNumber y2
                      :<| GFXNumber x3 :<| GFXNumber y3
                      :<| Empty) -> do
    precision <- usefulGraphicsPrecisionS
    let x2' = round' precision x2
        y2' = round' precision y2
        x3' = round' precision x3
        y3' = round' precision y3
    setCurrentPointS x3' y3'

    let optimizedCommand =
          Command GSCubicBezierCurve2To
            (   GFXNumber x2' :<| GFXNumber y2'
            :<| GFXNumber x3' :<| GFXNumber y3'
            :<| Empty
            )

    return $ replaceCommandWith command optimizedCommand

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
              then replaceCommandWith command (Command GSCloseSubpath mempty)
              else replaceCommandWith command optimized
          (Command GSCloseSubpath _params :<| _tail) ->
            if (x, y) == start
              then DeleteCommand
              else replaceCommandWith command optimized
          _otherCommand -> replaceCommandWith command optimized

  -- EndPath operator
  (GSEndPath, _noParameter) -> do
    setPathStartS 0 0
    return KeepCommand

  -- CloseSubpath operator
  (GSCloseSubpath, _noParameter) -> do
    pathStartX <- gets (gsPathStartX . iGraphicsState)
    pathStartY <- gets (gsPathStartY . iGraphicsState)
    setCurrentPointS pathStartX pathStartY

    return $ case rest of
      (Command GSStrokePath _noParameter :<| _tail) ->
        ReplaceAndDeleteNextCommand (Command GSCloseStrokePath mempty)
      (Command GSFillStrokePathEOR _noParameter :<| _tail ) ->
        ReplaceAndDeleteNextCommand (Command GSCloseFillStrokeEOR mempty)
      (Command GSFillStrokePathNZWR _noParameter :<| _tail) ->
        ReplaceAndDeleteNextCommand (Command GSCloseFillStrokeNZWR mempty)
      _anyOtherCommand -> KeepCommand

  -- Close and stroke the path operator
  (GSCloseStrokePath, _noParameter) -> do
    pathStartX <- gets (gsPathStartX . iGraphicsState)
    pathStartY <- gets (gsPathStartY . iGraphicsState)
    setCurrentPointS pathStartX pathStartY
    return KeepCommand

  (GSCloseFillStrokeEOR, _noParameter) -> do
    pathStartX <- gets (gsPathStartX . iGraphicsState)
    pathStartY <- gets (gsPathStartY . iGraphicsState)
    setCurrentPointS pathStartX pathStartY
    return KeepCommand

  (GSCloseFillStrokeNZWR, _noParameter) -> do
    pathStartX <- gets (gsPathStartX . iGraphicsState)
    pathStartY <- gets (gsPathStartY . iGraphicsState)
    setCurrentPointS pathStartX pathStartY
    return KeepCommand

  _anyOtherCommand -> return KeepCommand
 where
  operator   = cOperator command
  parameters = cParameters command
