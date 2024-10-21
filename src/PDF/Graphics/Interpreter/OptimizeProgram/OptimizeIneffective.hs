module PDF.Graphics.Interpreter.OptimizeProgram.OptimizeIneffective
  ( optimizeIneffective
  ) where

import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
    ( GSOperator (GSBeginCompatibilitySection, GSBeginInlineImage, GSBeginMarkedContentSequence, GSBeginMarkedContentSequencePL, GSBeginText, GSCloseFillStrokeEOR, GSCloseFillStrokeNZWR, GSCloseStrokePath, GSCloseSubpath, GSCubicBezierCurve, GSCubicBezierCurve1To, GSCubicBezierCurve2To, GSEndCompatibilitySection, GSEndInlineImage, GSEndMarkedContentSequence, GSEndPath, GSEndText, GSFillPathEOR, GSFillPathNZWR, GSFillStrokePathEOR, GSFillStrokePathNZWR, GSInlineImageData, GSIntersectClippingPathEOR, GSIntersectClippingPathNZWR, GSLineTo, GSMarkedContentPoint, GSMarkedContentPointPL, GSMoveTo, GSMoveToNextLine, GSMoveToNextLineLP, GSNLShowText, GSNLShowTextWithSpacing, GSNextLine, GSPaintShapeColourShading, GSPaintXObject, GSRectangle, GSRestoreGS, GSSaveGS, GSSetBoundingBoxGlyph, GSSetCTM, GSSetCharacterSpacing, GSSetColourRenderingIntent, GSSetFlatnessTolerance, GSSetGlyphWidth, GSSetHorizontalScaling, GSSetLineCap, GSSetLineDashPattern, GSSetLineJoin, GSSetLineWidth, GSSetMiterLimit, GSSetNonStrokeCMYKColorspace, GSSetNonStrokeColor, GSSetNonStrokeColorN, GSSetNonStrokeColorspace, GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetParameters, GSSetStrokeCMYKColorspace, GSSetStrokeColor, GSSetStrokeColorN, GSSetStrokeColorspace, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace, GSSetTextFont, GSSetTextLeading, GSSetTextMatrix, GSSetTextRenderingMode, GSSetTextRise, GSSetWordSpacing, GSShowManyText, GSShowText, GSStrokePath)
    )
import Data.PDF.Program (Program)
import Data.Sequence (Seq (Empty, (:<|)), (<|))


isRestore :: Command -> Bool
isRestore (Command GSRestoreGS _params) = True
isRestore _anyOtherCommand              = False

isDrawingCommand :: Command -> Bool
isDrawingCommand (Command command _params) = case command of
  GSSaveGS                       -> True
  GSRestoreGS                    -> True
  GSSetCTM                       -> False
  GSSetLineWidth                 -> False
  GSSetLineCap                   -> False
  GSSetLineJoin                  -> False
  GSSetMiterLimit                -> False
  GSSetLineDashPattern           -> False
  GSSetColourRenderingIntent     -> False
  GSSetFlatnessTolerance         -> False
  GSSetParameters                -> False
  GSMoveTo                       -> False
  GSLineTo                       -> True
  GSCubicBezierCurve             -> True
  GSCubicBezierCurve1To          -> True
  GSCubicBezierCurve2To          -> True
  GSCloseSubpath                 -> False
  GSRectangle                    -> True
  GSStrokePath                   -> True
  GSCloseStrokePath              -> True
  GSFillPathNZWR                 -> True
  GSFillPathEOR                  -> True
  GSFillStrokePathNZWR           -> True
  GSFillStrokePathEOR            -> True
  GSCloseFillStrokeNZWR          -> True
  GSCloseFillStrokeEOR           -> True
  GSEndPath                      -> True
  GSBeginText                    -> True
  GSEndText                      -> True
  GSMoveToNextLine               -> False
  GSMoveToNextLineLP             -> False
  GSSetTextMatrix                -> False
  GSNextLine                     -> False
  GSShowText                     -> True
  GSNLShowText                   -> True
  GSNLShowTextWithSpacing        -> True
  GSShowManyText                 -> True
  GSSetCharacterSpacing          -> False
  GSSetWordSpacing               -> False
  GSSetHorizontalScaling         -> False
  GSSetTextLeading               -> False
  GSSetTextFont                  -> False
  GSSetTextRenderingMode         -> False
  GSSetTextRise                  -> False
  GSSetGlyphWidth                -> False
  GSSetBoundingBoxGlyph          -> False
  GSSetStrokeColorspace          -> False
  GSSetNonStrokeColorspace       -> False
  GSSetStrokeColor               -> False
  GSSetStrokeColorN              -> False
  GSSetNonStrokeColor            -> False
  GSSetNonStrokeColorN           -> False
  GSSetStrokeGrayColorspace      -> False
  GSSetNonStrokeGrayColorspace   -> False
  GSSetStrokeRGBColorspace       -> False
  GSSetNonStrokeRGBColorspace    -> False
  GSSetStrokeCMYKColorspace      -> False
  GSSetNonStrokeCMYKColorspace   -> False
  GSPaintShapeColourShading      -> False
  GSPaintXObject                 -> True
  GSMarkedContentPoint           -> False
  GSMarkedContentPointPL         -> False
  GSBeginMarkedContentSequence   -> False
  GSBeginMarkedContentSequencePL -> False
  GSEndMarkedContentSequence     -> False
  GSBeginCompatibilitySection    -> False
  GSEndCompatibilitySection      -> False
  GSIntersectClippingPathNZWR    -> True
  GSIntersectClippingPathEOR     -> True
  GSBeginInlineImage             -> True
  GSInlineImageData              -> True
  GSEndInlineImage               -> True
  _anyOtherCommand               -> True

anyDrawingCommandBeforeRestore :: Program -> Bool
anyDrawingCommandBeforeRestore Empty = True
anyDrawingCommandBeforeRestore (command :<| rest)
  | isRestore command        = False
  | isDrawingCommand command = True
  | otherwise                = anyDrawingCommandBeforeRestore rest

{- |
Remove useless save/restore commands.
-}
optimizeIneffective :: Program -> Program
optimizeIneffective Empty = mempty
optimizeIneffective (command :<| rest)
  | isDrawingCommand command = command <| optimizeIneffective rest
  | otherwise =
      if anyDrawingCommandBeforeRestore rest
        then command <| optimizeIneffective rest
        else optimizeIneffective rest
