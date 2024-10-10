module Pdf.Graphics.Interpreter.OptimizeCommand
  ( optimizeCommand
  ) where

import Control.Monad.State (State, gets)

import Data.Functor ((<&>))
import Data.Sequence (Seq (Empty, (:<|)))

import Pdf.Graphics.Interpreter.Command
    ( Command (Command, cOperator, cParameters)
    )
import Pdf.Graphics.Interpreter.GraphicsState
    ( GraphicsState (gsLineCap, gsLineJoin, gsLineWidth, gsMiterLimit, gsIntent, gsFlatness)
    , applyGraphicsMatrixS
    , applyTextMatrixS
    , getPathStartS
    , restoreStateS
    , saveStateS
    , setCurrentPointS
    , setFontS
    , setHorizontalScalingS
    , setLineCapS
    , setLineJoinS
    , setLineWidthS
    , setMiterLimitS
    , setPathStartS
    , setTextRiseS
    , usefulColorPrecisionS
    , usefulGraphicsPrecisionS
    , usefulTextPrecisionS, setRenderingIntentS, setFlatnessS
    )
import Pdf.Graphics.Interpreter.OperatorCategory
    ( OperatorCategory (ClippingPathOperator, ColorOperator, PathConstructionOperator, PathPaintingOperator, TextPositioningOperator, TextShowingOperator, TextStateOperator, Type3FontOperator)
    , category
    )
import Pdf.Graphics.Interpreter.Program (Program)
import Pdf.Graphics.Interpreter.TransformationMatrix
    ( TransformationMatrix (TransformationMatrix)
    )
import Pdf.Graphics.Object
    ( GFXObject (GFXName, GFXNull, GFXNumber)
    , GSOperator (GSCloseSubpath, GSEndPath, GSLineTo, GSMoveTo, GSNone, GSRestoreGS, GSSaveGS, GSSetCTM, GSSetHorizontalScaling, GSSetLineCap, GSSetLineJoin, GSSetLineWidth, GSSetMiterLimit, GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace, GSSetTextFont, GSSetTextMatrix, GSSetTextRise, GSSetColourRenderingIntent, GSSetFlatnessTolerance)
    , reducePrecision
    )
import Pdf.Graphics.Objects (Objects)

import Util.Number (roundAndAHalf)


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
    parameters :: Objects
    parameters = cParameters command

    first :: Objects -> GFXObject
    first (firstItem :<| _remain) = firstItem
    first _otherParameters        = GFXNull

    isGray :: Objects -> Bool
    isGray (a :<| b :<| c :<| Empty) = a == b && b == c
    isGray _otherParameters          = False

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

  -- Set current transformation matrix
  (GSSetCTM, GFXNumber a :<| GFXNumber b :<| GFXNumber c :<| GFXNumber d :<| GFXNumber e :<| GFXNumber f :<| Empty) -> do
    applyGraphicsMatrixS (TransformationMatrix a b c d e f)
    usefulGraphicsPrecisionS <&> optimizeParameters command . (+ 2)

  -- Set text transformation matrix
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

  -- MoveTo operator
  (GSMoveTo, GFXNumber x :<| GFXNumber y :<| Empty) -> do
    setPathStartS x y
    usefulGraphicsPrecisionS <&> optimizeParameters command

  -- LineTo operator
  (GSLineTo, GFXNumber x :<| GFXNumber y :<| Empty) -> do
    setCurrentPointS x y
    optimized <- usefulGraphicsPrecisionS <&> optimizeParameters command
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
    width' <- usefulGraphicsPrecisionS <&> flip roundAndAHalf width
    currentWidth <- gets gsLineWidth
    if width' == currentWidth
      then return (Command GSNone mempty)
      else do
        setLineWidthS width'
        usefulGraphicsPrecisionS <&> optimizeParameters command

  (GSSetLineCap, GFXNumber lineCap :<| Empty) -> do
    lineCap' <- usefulGraphicsPrecisionS <&> flip roundAndAHalf lineCap
    currentLineCap <- gets gsLineCap
    if lineCap' == currentLineCap
      then return (Command GSNone mempty)
      else do
        setLineCapS lineCap'
        usefulGraphicsPrecisionS <&> optimizeParameters command

  (GSSetLineJoin, GFXNumber lineJoin :<| Empty) -> do
    lineJoin' <- usefulGraphicsPrecisionS <&> flip roundAndAHalf lineJoin
    currentLineJoin <- gets gsLineJoin
    if lineJoin' == currentLineJoin
      then return (Command GSNone mempty)
      else do
        setLineJoinS lineJoin'
        usefulGraphicsPrecisionS <&> optimizeParameters command

  (GSSetMiterLimit, GFXNumber miterLimit :<| Empty) -> do
    miterLimit' <- usefulGraphicsPrecisionS <&> flip roundAndAHalf miterLimit
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
    flatness' <- usefulGraphicsPrecisionS <&> flip roundAndAHalf flatness
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
