module Pdf.Graphics.Interpreter.OperatorCategory
  ( OperatorCategory (GeneralGraphicsStateOperator, SpecialGraphicsStateOperator, PathConstructionOperator, PathPaintingOperator, ClippingPathOperator, TextObjectOperator, TextStateOperator, TextPositioningOperator, TextShowingOperator, Type3FontOperator, ColorOperator, ShadingOperator, InlineImageOperator, XObjectOperator, MarkedContentOperator, CompatibilityOperator)
  , category
  )
where

import Data.Kind (Type)
import Data.PDF.GFXObject
    ( GSOperator (GSBeginCompatibilitySection, GSBeginMarkedContentSequence, GSBeginMarkedContentSequencePL, GSBeginText, GSCloseFillStrokeEOR, GSCloseFillStrokeNZWR, GSCloseStrokePath, GSCloseSubpath, GSCubicBezierCurve, GSCubicBezierCurve1To, GSCubicBezierCurve2To, GSEndCompatibilitySection, GSEndMarkedContentSequence, GSEndPath, GSEndText, GSFillPathEOR, GSFillPathNZWR, GSFillStrokePathEOR, GSFillStrokePathNZWR, GSIntersectClippingPathEOR, GSIntersectClippingPathNZWR, GSLineTo, GSMarkedContentPoint, GSMarkedContentPointPL, GSMoveTo, GSMoveToNextLine, GSMoveToNextLineLP, GSNLShowText, GSNLShowTextWithSpacing, GSNextLine, GSPaintShapeColourShading, GSPaintXObject, GSRectangle, GSRestoreGS, GSSaveGS, GSSetBoundingBoxGlyph, GSSetCTM, GSSetCharacterSpacing, GSSetColourRenderingIntent, GSSetFlatnessTolerance, GSSetGlyphWidth, GSSetHorizontalScaling, GSSetLineCap, GSSetLineDashPattern, GSSetLineJoin, GSSetLineWidth, GSSetMiterLimit, GSSetNonStrokeCMYKColorspace, GSSetNonStrokeColor, GSSetNonStrokeColorN, GSSetNonStrokeColorspace, GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetParameters, GSSetStrokeCMYKColorspace, GSSetStrokeColor, GSSetStrokeColorN, GSSetStrokeColorspace, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace, GSSetTextFont, GSSetTextLeading, GSSetTextMatrix, GSSetTextRenderingMode, GSSetTextRise, GSSetWordSpacing, GSShowManyText, GSShowText, GSStrokePath)
    )

type OperatorCategory :: Type
data OperatorCategory
  = GeneralGraphicsStateOperator
  | SpecialGraphicsStateOperator
  | PathConstructionOperator
  | PathPaintingOperator
  | ClippingPathOperator
  | TextObjectOperator
  | TextStateOperator
  | TextPositioningOperator
  | TextShowingOperator
  | Type3FontOperator
  | ColorOperator
  | ShadingOperator
  | InlineImageOperator
  | XObjectOperator
  | MarkedContentOperator
  | CompatibilityOperator
  deriving stock (Eq, Show)

category :: GSOperator -> OperatorCategory
category GSSaveGS                       = GeneralGraphicsStateOperator
category GSRestoreGS                    = GeneralGraphicsStateOperator
category GSSetLineWidth                 = GeneralGraphicsStateOperator
category GSSetLineCap                   = GeneralGraphicsStateOperator
category GSSetLineJoin                  = GeneralGraphicsStateOperator
category GSSetMiterLimit                = GeneralGraphicsStateOperator
category GSSetLineDashPattern           = GeneralGraphicsStateOperator
category GSSetColourRenderingIntent     = GeneralGraphicsStateOperator
category GSSetFlatnessTolerance         = GeneralGraphicsStateOperator
category GSSetParameters                = GeneralGraphicsStateOperator
category GSSetCTM                       = SpecialGraphicsStateOperator
category GSMoveTo                       = PathConstructionOperator
category GSLineTo                       = PathConstructionOperator
category GSCubicBezierCurve             = PathConstructionOperator
category GSCubicBezierCurve1To          = PathConstructionOperator
category GSCubicBezierCurve2To          = PathConstructionOperator
category GSCloseSubpath                 = PathConstructionOperator
category GSRectangle                    = PathConstructionOperator
category GSStrokePath                   = PathPaintingOperator
category GSCloseStrokePath              = PathPaintingOperator
category GSFillPathNZWR                 = PathPaintingOperator
category GSFillPathEOR                  = PathPaintingOperator
category GSFillStrokePathNZWR           = PathPaintingOperator
category GSFillStrokePathEOR            = PathPaintingOperator
category GSCloseFillStrokeNZWR          = PathPaintingOperator
category GSCloseFillStrokeEOR           = PathPaintingOperator
category GSEndPath                      = PathPaintingOperator
category GSBeginText                    = TextObjectOperator
category GSEndText                      = TextObjectOperator
category GSMoveToNextLine               = TextStateOperator
category GSMoveToNextLineLP             = TextStateOperator
category GSSetTextMatrix                = TextStateOperator
category GSNextLine                     = TextStateOperator
category GSShowText                     = TextShowingOperator
category GSNLShowText                   = TextShowingOperator
category GSNLShowTextWithSpacing        = TextShowingOperator
category GSShowManyText                 = TextShowingOperator
category GSSetCharacterSpacing          = TextStateOperator
category GSSetWordSpacing               = TextStateOperator
category GSSetHorizontalScaling         = TextStateOperator
category GSSetTextLeading               = TextStateOperator
category GSSetTextFont                  = TextStateOperator
category GSSetTextRenderingMode         = TextStateOperator
category GSSetTextRise                  = TextStateOperator
category GSSetGlyphWidth                = Type3FontOperator
category GSSetBoundingBoxGlyph          = Type3FontOperator
category GSSetStrokeColorspace          = ColorOperator
category GSSetNonStrokeColorspace       = ColorOperator
category GSSetStrokeColor               = ColorOperator
category GSSetStrokeColorN              = ColorOperator
category GSSetNonStrokeColor            = ColorOperator
category GSSetNonStrokeColorN           = ColorOperator
category GSSetStrokeGrayColorspace      = ColorOperator
category GSSetNonStrokeGrayColorspace   = ColorOperator
category GSSetStrokeRGBColorspace       = ColorOperator
category GSSetNonStrokeRGBColorspace    = ColorOperator
category GSSetStrokeCMYKColorspace      = ColorOperator
category GSSetNonStrokeCMYKColorspace   = ColorOperator
category GSPaintShapeColourShading      = ShadingOperator
category GSPaintXObject                 = XObjectOperator
category GSMarkedContentPoint           = MarkedContentOperator
category GSMarkedContentPointPL         = MarkedContentOperator
category GSBeginMarkedContentSequence   = MarkedContentOperator
category GSBeginMarkedContentSequencePL = MarkedContentOperator
category GSEndMarkedContentSequence     = MarkedContentOperator
category GSBeginCompatibilitySection    = CompatibilityOperator
category GSEndCompatibilitySection      = CompatibilityOperator
category GSIntersectClippingPathNZWR    = ClippingPathOperator
category GSIntersectClippingPathEOR     = ClippingPathOperator
category _anyOtherOperator              = GeneralGraphicsStateOperator
