module PDF.Graphics.Interpreter.OptimizeCommand
  ( optimizeCommand
  ) where

import Control.Monad.State (State, gets)

import Data.Functor ((<&>))
import Data.PDF.Command (Command (Command, cOperator, cParameters))
import Data.PDF.GFXObject
    ( GFXObject (GFXArray, GFXHexString, GFXName, GFXNumber, GFXString)
    , GSOperator (GSBeginText, GSCloseSubpath, GSCubicBezierCurve, GSCubicBezierCurve1To, GSCubicBezierCurve2To, GSEndPath, GSEndText, GSLineTo, GSMoveTo, GSMoveToNextLine, GSMoveToNextLineLP, GSRestoreGS, GSSaveGS, GSSetCTM, GSSetColourRenderingIntent, GSSetFlatnessTolerance, GSSetHorizontalScaling, GSSetLineCap, GSSetLineJoin, GSSetLineWidth, GSSetMiterLimit, GSSetNonStrokeCMYKColorspace, GSSetNonStrokeColor, GSSetNonStrokeColorN, GSSetNonStrokeColorspace, GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetStrokeCMYKColorspace, GSSetStrokeColor, GSSetStrokeColorN, GSSetStrokeColorspace, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace, GSSetTextFont, GSSetTextMatrix, GSSetTextRise, GSShowManyText, GSShowText)
    )
import Data.PDF.GraphicsState
    ( GraphicsState (gsCurrentPointX, gsCurrentPointY, gsFlatness, gsIntent, gsLineCap, gsLineJoin, gsLineWidth, gsMiterLimit, gsPathStartX, gsPathStartY, gsStrokeColor)
    , gsNonStrokeColor
    )
import Data.PDF.InterpreterAction
    ( InterpreterAction (DeleteCommand, KeepCommand, ReplaceAndDeleteNextCommand, ReplaceCommand)
    )
import Data.PDF.InterpreterState
    ( InterpreterState (iGraphicsState)
    , applyGraphicsMatrixS
    , applyTextMatrixS
    , getPathStartS
    , resetTextStateS
    , restoreStateS
    , saveStateS
    , setCurrentPointS
    , setFlatnessS
    , setFontS
    , setHorizontalScalingS
    , setLineCapS
    , setLineJoinS
    , setLineWidthS
    , setMiterLimitS
    , setNonStrokeColorS
    , setPathStartS
    , setRenderingIntentS
    , setStrokeColorS
    , setTextRiseS
    , usefulColorPrecisionS
    , usefulGraphicsPrecisionS
    , usefulTextPrecisionS
    )
import Data.PDF.OperatorCategory
    ( OperatorCategory (ClippingPathOperator, ColorOperator, PathConstructionOperator, PathPaintingOperator, TextPositioningOperator, TextShowingOperator, TextStateOperator, Type3FontOperator)
    , category
    )
import Data.PDF.Program (Program)
import Data.PDF.TransformationMatrix
    ( TransformationMatrix (TransformationMatrix)
    )
import Data.Sequence (Seq (Empty, (:<|)))

import PDF.Graphics.Interpreter.OptimizeColor
    ( mkColor
    , mkNonStrokeCommand
    , mkStrokeCommand
    , optimizeColorCommand
    )
import PDF.Graphics.Interpreter.OptimizeParameters (optimizeParameters)

import Util.Graphics (areAligned)
import Util.Number (round')


hasTextmoveBeforeEndText :: Program -> Bool
hasTextmoveBeforeEndText (Command GSEndPath _params :<| _tail) = False
hasTextmoveBeforeEndText (Command GSCloseSubpath _params :<| _tail) = False
hasTextmoveBeforeEndText (Command GSMoveToNextLine _params :<| _tail) = True
hasTextmoveBeforeEndText (Command GSMoveToNextLineLP _params :<| _tail) = True
hasTextmoveBeforeEndText (Command GSSetTextMatrix _params :<| _tail) = True
hasTextmoveBeforeEndText (_command :<| rest) = hasTextmoveBeforeEndText rest
hasTextmoveBeforeEndText Empty = False

{- |
The 'optimizeCommand' function takes a 'GraphicsState' and a 'Command' and
returns an optimized 'Command'.
-}
optimizeCommand
  :: Command
  -> Program
  -> State InterpreterState InterpreterAction
optimizeCommand command rest = case (operator, parameters) of
  -- Save graphics state
  (GSSaveGS, _params) -> do
    saveStateS
    return KeepCommand

  -- Restore graphics state
  (GSRestoreGS, _params) -> do
    restoreStateS
    return KeepCommand

  -- Begin text object
  (GSBeginText, _params) -> do
    resetTextStateS
    applyTextMatrixS mempty
    return KeepCommand

  -- End text object
  (GSEndText, _params) -> return KeepCommand

  -- Identity matrix can be ignored
  (GSSetCTM, GFXNumber 1
         :<| GFXNumber 0
         :<| GFXNumber 0
         :<| GFXNumber 1
         :<| GFXNumber 0
         :<| GFXNumber 0
         :<| Empty) -> do
    return DeleteCommand

  -- Set current transformation matrix
  (GSSetCTM, GFXNumber a
         :<| GFXNumber b
         :<| GFXNumber c
         :<| GFXNumber d
         :<| GFXNumber e
         :<| GFXNumber f
         :<| Empty) -> do
    precision <- usefulGraphicsPrecisionS <&> (+ 1)
    case rest of
      (Command GSSetCTM ( GFXNumber a'
                      :<| GFXNumber b'
                      :<| GFXNumber c'
                      :<| GFXNumber d'
                      :<| GFXNumber e'
                      :<| GFXNumber f'
                      :<| Empty)
                      :<| _tail) -> do
        -- Merge two consecutive SetCTM operators
        applyGraphicsMatrixS (TransformationMatrix a b c d e f)
        applyGraphicsMatrixS (TransformationMatrix a' b' c' d' e' f')
        let command' = Command GSSetCTM (   GFXNumber (a * a')
                                        :<| GFXNumber (b * a')
                                        :<| GFXNumber (c * a')
                                        :<| GFXNumber (d * a')
                                        :<| GFXNumber (e * a' + f)
                                        :<| GFXNumber (f * a' + f')
                                        :<| Empty
                                        )
        return $ ReplaceAndDeleteNextCommand (optimizeParameters command' precision)
      _anythingElse -> do
        applyGraphicsMatrixS (TransformationMatrix a b c d e f)
        return $ ReplaceCommand (optimizeParameters command precision)

  -- Identity text matrix can be ignored
  (GSSetTextMatrix, GFXNumber 1
                :<| GFXNumber 0
                :<| GFXNumber 0
                :<| GFXNumber 1
                :<| GFXNumber 0
                :<| GFXNumber 0
                :<| Empty) ->
    return DeleteCommand

  -- Set text transformation matrix
  (GSSetTextMatrix, GFXNumber 1
                :<| GFXNumber 0
                :<| GFXNumber 0
                :<| GFXNumber 1
                :<| GFXNumber tx
                :<| GFXNumber ty
                :<| Empty) -> do
    precision <- usefulTextPrecisionS
    if hasTextmoveBeforeEndText rest
      then do
        applyTextMatrixS (TransformationMatrix 1 0 0 1 tx ty)
        return $ ReplaceCommand (optimizeParameters command precision)
      else
        return $ ReplaceCommand
                  ( optimizeParameters
                    (Command GSMoveToNextLine ( GFXNumber tx
                                            :<| GFXNumber ty
                                            :<| Empty)
                    )
                    precision
                  )

  (GSSetTextMatrix, GFXNumber a
                :<| GFXNumber b
                :<| GFXNumber c
                :<| GFXNumber d
                :<| GFXNumber e
                :<| GFXNumber f
                :<| Empty) -> do
    applyTextMatrixS (TransformationMatrix a b c d e f)
    ReplaceCommand . optimizeParameters command <$> usefulTextPrecisionS

  -- Set text font and size
  (GSSetTextFont, GFXName fontName :<| GFXNumber fontSize :<| Empty) -> do
    setFontS fontName fontSize
    ReplaceCommand . optimizeParameters command <$> usefulGraphicsPrecisionS

  -- Set text horizontal scaling
  (GSSetHorizontalScaling, GFXNumber scaling :<| Empty) -> do
    setHorizontalScalingS scaling
    ReplaceCommand . optimizeParameters command <$> usefulGraphicsPrecisionS

  -- Set text rise
  (GSSetTextRise, GFXNumber rise :<| Empty) -> do
    setTextRiseS rise
    ReplaceCommand . optimizeParameters command <$> usefulTextPrecisionS

  -- Replace ShowManyText by ShowText when there is only one text
  (GSShowManyText, GFXArray items :<| Empty) -> do
    case items of
      str@(GFXString _string :<| Empty) ->
        return $ ReplaceCommand (Command GSShowText str)
      str@(GFXHexString _string :<| Empty) ->
        return $ ReplaceCommand (Command GSShowText str)
      _otherContent ->
        ReplaceCommand . optimizeParameters command <$> usefulTextPrecisionS

  -- MoveTo operator
  (GSMoveTo, GFXNumber x :<| GFXNumber y :<| Empty) -> do
    setPathStartS x y
    ReplaceCommand . optimizeParameters command <$> usefulGraphicsPrecisionS

  -- Keep track of current position
  (GSCubicBezierCurve, GFXNumber _x1 :<| GFXNumber _y1
                   :<| GFXNumber _x2 :<| GFXNumber _y2
                   :<| GFXNumber x3  :<| GFXNumber y3
                   :<| Empty) -> do
    setCurrentPointS x3 y3
    ReplaceCommand . optimizeParameters command <$> usefulGraphicsPrecisionS

  (GSCubicBezierCurve1To, GFXNumber _x1 :<| GFXNumber _y1
                      :<| GFXNumber x3  :<| GFXNumber y3
                      :<| Empty) -> do
    setCurrentPointS x3 y3
    ReplaceCommand . optimizeParameters command <$> usefulGraphicsPrecisionS

  (GSCubicBezierCurve2To, GFXNumber _x2 :<| GFXNumber _y2
                      :<| GFXNumber x3  :<| GFXNumber y3
                      :<| Empty) -> do
    setCurrentPointS x3 y3
    ReplaceCommand . optimizeParameters command <$> usefulGraphicsPrecisionS

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

  (GSSetColourRenderingIntent, GFXName intent :<| Empty) -> do
    currentIntent <- gets (gsIntent . iGraphicsState)
    if intent == currentIntent
      then return DeleteCommand
      else do
        setRenderingIntentS intent
        return KeepCommand

  (GSSetFlatnessTolerance, GFXNumber flatness :<| Empty) -> do
    flatness' <- usefulGraphicsPrecisionS <&> flip round' flatness
    currentFlatness <- gets (gsFlatness . iGraphicsState)
    if flatness' == currentFlatness
      then return DeleteCommand
      else do
        setFlatnessS flatness'
        ReplaceCommand . optimizeParameters command <$> usefulGraphicsPrecisionS

  (GSSetStrokeColorspace, _parameters) -> do
    currentColor <- gets (gsStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkStrokeCommand newColor)

  (GSSetNonStrokeColorspace, _parameters) -> do
    currentColor <- gets (gsNonStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setNonStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkNonStrokeCommand newColor)

  (GSSetStrokeColor, _parameters) -> do
    currentColor <- gets (gsStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkStrokeCommand newColor)

  (GSSetNonStrokeColor, _parameters) -> do
    currentColor <- gets (gsNonStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setNonStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkNonStrokeCommand newColor)

  (GSSetStrokeColorN, _parameters) -> do
    currentColor <- gets (gsStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkStrokeCommand newColor)

  (GSSetNonStrokeColorN, _parameters) -> do
    currentColor <- gets (gsNonStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setNonStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkNonStrokeCommand newColor)

  (GSSetStrokeGrayColorspace, _parameters) -> do
    currentColor <- gets (gsStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkStrokeCommand newColor)

  (GSSetNonStrokeGrayColorspace, _parameters) -> do
    currentColor <- gets (gsNonStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setNonStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkNonStrokeCommand newColor)

  (GSSetStrokeRGBColorspace, _parameters) -> do
    currentColor <- gets (gsStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkStrokeCommand newColor)

  (GSSetNonStrokeRGBColorspace, _parameters) -> do
    currentColor <- gets (gsNonStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setNonStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkNonStrokeCommand newColor)

  (GSSetStrokeCMYKColorspace, _parameters) -> do
    currentColor <- gets (gsStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkStrokeCommand newColor)

  (GSSetNonStrokeCMYKColorspace, _parameters) -> do
    currentColor <- gets (gsNonStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setNonStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkNonStrokeCommand newColor)

  -- Other operators
  _otherOperator -> case category operator of
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
                   . optimizeColorCommand
                   . optimizeParameters command
                 <$> usefulColorPrecisionS

    _otherOperatorCategory -> return KeepCommand
 where
  operator   = cOperator command
  parameters = cParameters command
