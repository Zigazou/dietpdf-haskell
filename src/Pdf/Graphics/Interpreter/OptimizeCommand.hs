module Pdf.Graphics.Interpreter.OptimizeCommand
  ( optimizeCommand
  ) where

import Control.Monad.State (State, gets)

import Data.Functor ((<&>))
import Data.PDF.GFXObject
    ( GFXObject (GFXArray, GFXHexString, GFXName, GFXNull, GFXNumber, GFXString)
    , GSOperator (GSBeginText, GSCloseSubpath, GSEndPath, GSEndText, GSLineTo, GSMoveTo, GSMoveToNextLine, GSMoveToNextLineLP, GSNone, GSRestoreGS, GSSaveGS, GSSetCTM, GSSetColourRenderingIntent, GSSetFlatnessTolerance, GSSetHorizontalScaling, GSSetLineCap, GSSetLineJoin, GSSetLineWidth, GSSetMiterLimit, GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace, GSSetTextFont, GSSetTextMatrix, GSSetTextRise, GSShowManyText, GSShowText)
    , reducePrecision
    )
import Data.PDF.GFXObjects (GFXObjects)
import Data.Sequence (Seq (Empty, (:<|)))

import Pdf.Graphics.Interpreter.Command
    ( Command (Command, cOperator, cParameters)
    )
import Pdf.Graphics.Interpreter.GraphicsState
    ( GraphicsState (gsCurrentPointX, gsCurrentPointY, gsFlatness, gsIntent, gsLineCap, gsLineJoin, gsLineWidth, gsMiterLimit, gsPathStartX, gsPathStartY)
    , applyGraphicsMatrixS
    , applyTextMatrixS
    , getPathStartS
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
    , setPathStartS
    , setRenderingIntentS
    , setTextRiseS
    , usefulColorPrecisionS
    , usefulGraphicsPrecisionS
    , usefulTextPrecisionS
    )
import Pdf.Graphics.Interpreter.OperatorCategory
    ( OperatorCategory (ClippingPathOperator, ColorOperator, PathConstructionOperator, PathPaintingOperator, TextPositioningOperator, TextShowingOperator, TextStateOperator, Type3FontOperator)
    , category
    )
import Pdf.Graphics.Interpreter.Program (Program)
import Pdf.Graphics.Interpreter.TransformationMatrix
    ( TransformationMatrix (TransformationMatrix)
    )

import Util.Graphics (areAligned)
import Util.Number (round')


optimizeParameters :: Command -> Int -> Command
optimizeParameters command precision =
  command { cParameters = reducePrecision precision <$> cParameters command}

optimizeColorCommand :: Command -> Command
optimizeColorCommand command
  | isGray (cParameters command) = case cOperator command of
      GSSetStrokeRGBColorspace ->
        command { cOperator = GSSetStrokeGrayColorspace
                , cParameters = first parameters :<| Empty
                }
      GSSetNonStrokeRGBColorspace ->
        command { cOperator = GSSetNonStrokeGrayColorspace
                , cParameters = first parameters :<| Empty
                }
      _anyOtherOperator -> command
  | otherwise = command
  where
    parameters :: GFXObjects
    parameters = cParameters command

    first :: GFXObjects -> GFXObject
    first (firstItem :<| _remain) = firstItem
    first _otherParameters        = GFXNull

    isGray :: GFXObjects -> Bool
    isGray (a :<| b :<| c :<| Empty) = a == b && b == c
    isGray _otherParameters          = False

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
optimizeCommand :: Command -> Program -> State GraphicsState Command
optimizeCommand command rest = case (operator, parameters) of
  -- Save graphics state
  (GSSaveGS, _params) -> saveStateS >> return command

  -- Restore graphics state
  (GSRestoreGS, _params) -> restoreStateS >> return command

  -- Begin text object
  (GSBeginText, _params) -> applyTextMatrixS mempty >> return command

  -- End text object
  (GSEndText, _params) -> return command

  -- Set current transformation matrix
  (GSSetCTM, GFXNumber a :<| GFXNumber b :<| GFXNumber c :<| GFXNumber d :<| GFXNumber e :<| GFXNumber f :<| Empty) -> do
    applyGraphicsMatrixS (TransformationMatrix a b c d e f)
    usefulGraphicsPrecisionS <&> optimizeParameters command . (+ 2)

  -- Set text transformation matrix
  (GSSetTextMatrix, GFXNumber 1.0 :<| GFXNumber 0.0 :<| GFXNumber 0.0 :<| GFXNumber 1.0 :<| GFXNumber tx :<| GFXNumber ty :<| Empty) -> do
    precision <- usefulTextPrecisionS
    if hasTextmoveBeforeEndText rest
      then return $ optimizeParameters command precision
      else
        return $ optimizeParameters
                  (Command GSMoveToNextLine (GFXNumber tx :<| GFXNumber ty :<| Empty))
                  precision

  (GSSetTextMatrix, GFXNumber a :<| GFXNumber b :<| GFXNumber c :<| GFXNumber d :<| GFXNumber e :<| GFXNumber f :<| Empty) -> do
    applyTextMatrixS (TransformationMatrix a b c d e f)
    usefulTextPrecisionS <&> optimizeParameters command

  -- Set text font and size
  (GSSetTextFont, GFXName fontName :<| GFXNumber fontSize :<| Empty) -> do
    setFontS fontName fontSize
    usefulGraphicsPrecisionS <&> optimizeParameters command

  -- Set text horizontal scaling
  (GSSetHorizontalScaling, GFXNumber scaling :<| Empty) -> do
    setHorizontalScalingS scaling
    usefulGraphicsPrecisionS <&> optimizeParameters command

  -- Set text rise
  (GSSetTextRise, GFXNumber rise :<| Empty) -> do
    setTextRiseS rise
    usefulTextPrecisionS <&> optimizeParameters command

  -- Replace ShowManyText by ShowText when there is only one text
  (GSShowManyText, GFXArray items :<| Empty) -> do
    case items of
      str@(GFXString _string :<| Empty) -> return (Command GSShowText str)
      str@(GFXHexString _string :<| Empty) -> return (Command GSShowText str)
      _otherContent -> usefulTextPrecisionS <&> optimizeParameters command

  -- MoveTo operator
  (GSMoveTo, GFXNumber x :<| GFXNumber y :<| Empty) -> do
    setPathStartS x y
    usefulGraphicsPrecisionS <&> optimizeParameters command

  -- Optimize LineTo operator
  (GSLineTo, GFXNumber x :<| GFXNumber y :<| Empty) -> do
    currentX <- gets gsCurrentPointX
    currentY <- gets gsCurrentPointY
    startX   <- gets gsPathStartX
    startY   <- gets gsPathStartY

    -- Calculate next coordinates.
    let lineIsNotNeeded =
          case rest of
            (Command GSEndPath _params :<| _tail) ->
              areAligned (currentX, currentY) (x, y) (startX, startY) 10
            (Command GSCloseSubpath _params :<| _tail) ->
              areAligned (currentX, currentY) (x, y) (startX, startY) 10
            (Command GSLineTo (GFXNumber nextX :<| GFXNumber nextY :<| Empty) :<| _tail) ->
              areAligned (currentX, currentY) (x, y) (nextX, nextY) 10
            _otherCommand -> False

    setCurrentPointS x y

    -- If the LineTo operator has coordinates on the line between the current
    -- point and the next coordinates, then remove the LineTo operator.
    if lineIsNotNeeded
      then return (Command GSNone mempty)
      else do
        optimized <- usefulGraphicsPrecisionS <&> optimizeParameters command

        -- If the LineTo operator is followed by a CloseSubpath operator and it
        -- goes back to the start of the path, then remove the LineTo operator
        -- because the CloseSubpath operator will close the path.
        case rest of
          (Command GSEndPath _params :<| _tail) -> do
            start <- getPathStartS
            if (x, y) == start
              then return (Command GSCloseSubpath mempty)
              else return optimized
          (Command GSCloseSubpath _params :<| _tail) -> do
            start <- getPathStartS
            if (x, y) == start
              then return (Command GSNone mempty)
              else return optimized
          _otherCommand -> return optimized

  -- EndPath operator
  (GSEndPath, _params) -> do
    setPathStartS 0 0
    return command

  -- CloseSubpath operator
  (GSCloseSubpath, _params) -> do
    setPathStartS 0 0
    return command

  (GSSetLineWidth, GFXNumber width :<| Empty) -> do
    width' <- usefulGraphicsPrecisionS <&> flip round' width
    currentWidth <- gets gsLineWidth
    if width' == currentWidth
      then return (Command GSNone mempty)
      else do
        setLineWidthS width'
        usefulGraphicsPrecisionS <&> optimizeParameters command

  (GSSetLineCap, GFXNumber lineCap :<| Empty) -> do
    lineCap' <- usefulGraphicsPrecisionS <&> flip round' lineCap
    currentLineCap <- gets gsLineCap
    if lineCap' == currentLineCap
      then return (Command GSNone mempty)
      else do
        setLineCapS lineCap'
        usefulGraphicsPrecisionS <&> optimizeParameters command

  (GSSetLineJoin, GFXNumber lineJoin :<| Empty) -> do
    lineJoin' <- usefulGraphicsPrecisionS <&> flip round' lineJoin
    currentLineJoin <- gets gsLineJoin
    if lineJoin' == currentLineJoin
      then return (Command GSNone mempty)
      else do
        setLineJoinS lineJoin'
        usefulGraphicsPrecisionS <&> optimizeParameters command

  (GSSetMiterLimit, GFXNumber miterLimit :<| Empty) -> do
    miterLimit' <- usefulGraphicsPrecisionS <&> flip round' miterLimit
    currentMiterLimit <- gets gsMiterLimit
    if miterLimit' == currentMiterLimit
      then return (Command GSNone mempty)
      else do
        setMiterLimitS miterLimit'
        usefulGraphicsPrecisionS <&> optimizeParameters command

  (GSSetColourRenderingIntent, GFXName intent :<| Empty) -> do
    currentIntent <- gets gsIntent
    if intent == currentIntent
      then return (Command GSNone mempty)
      else do
        setRenderingIntentS intent
        return command

  (GSSetFlatnessTolerance, GFXNumber flatness :<| Empty) -> do
    flatness' <- usefulGraphicsPrecisionS <&> flip round' flatness
    currentFlatness <- gets gsFlatness
    if flatness' == currentFlatness
      then return (Command GSNone mempty)
      else do
        setFlatnessS flatness'
        usefulGraphicsPrecisionS <&> optimizeParameters command

  -- Other operators
  _otherOperator -> case category operator of
    PathConstructionOperator -> usefulGraphicsPrecisionS <&> optimizeParameters command
    PathPaintingOperator     -> usefulGraphicsPrecisionS <&> optimizeParameters command
    ClippingPathOperator     -> usefulGraphicsPrecisionS <&> optimizeParameters command
    TextStateOperator        -> usefulTextPrecisionS <&> optimizeParameters command
    Type3FontOperator        -> usefulTextPrecisionS <&> optimizeParameters command
    TextPositioningOperator  -> usefulTextPrecisionS <&> optimizeParameters command
    TextShowingOperator      -> usefulTextPrecisionS <&> optimizeParameters command
    ColorOperator            -> usefulColorPrecisionS <&> optimizeColorCommand . optimizeParameters command

    _otherOperatorCategory   -> return command
 where
  operator   = cOperator command
  parameters = cParameters command
