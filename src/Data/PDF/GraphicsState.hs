{-|
Pure updates to the PDF graphics and text state.

This module models the parts of the PDF "graphics state" that are needed by
dietpdf when interpreting page content streams.

The functions in this module are /pure setters/ and /transformers/: they take a
'GraphicsState' value and return an updated copy. No IO is performed here.

In PDF terminology, the graphics state includes (among other parameters):

* The current transformation matrix (CTM).
* The current text state (text matrix, font, spacing, etc.).
* Stroke/non-stroke drawing parameters (line width, dash pattern, alpha, ...).

The "useful precision" helpers are heuristics used to choose a stable decimal
precision for output, based on the effective scale implied by the CTM and text
state.
-}
module Data.PDF.GraphicsState
  ( GraphicsState (..)
  , defaultGraphicsState
  , usefulGraphicsPrecision
  , usefulTextPrecision
  , usefulColorPrecision
  , applyGraphicsMatrix
  , setTextMatrix
  , setFont
  , setHorizontalScaling
  , setTextRise
  , setTextLeading
  , setCharacterSpacing
  , setWordSpacing
  , setPathStart
  , setNonStrokeAlpha
  , setStrokeAlpha
  , setFlatness
  , setRenderingIntent
  , setDashPattern
  , setMiterLimit
  , setLineJoin
  , setLineCap
  , setLineWidth
  , setCurrentPoint
  , resetTextState
  , setStrokeColor
  , setNonStrokeColor
  , usefulMatrixPrecisionFor
  , applyTextMatrix
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.PDF.Color (Color (ColorNotSet))
import Data.PDF.TextState
  ( TextState (tsFont, tsFontSize, tsHorizontalScaling, tsLeading, tsMatrix, tsRise)
  , defaultTextState
  , tsCharacterSpacing
  , tsScaleX
  , tsScaleY
  , tsWordSpacing
  )
import Data.PDF.TransformationMatrix
  ( TransformationMatrix (TransformationMatrix, tmA, tmB, tmC, tmD, tmE, tmF)
  , matrixScale
  )

{-|
In-memory representation of the (subset of the) PDF graphics state tracked by
dietpdf.

This is a "current" state: it changes as graphics/text operators are
interpreted.

The record also stores derived values (@gsScaleX@, @gsScaleY@) that represent
the absolute scaling implied by the CTM, which is used by the precision
heuristics.
-}
type GraphicsState :: Type
data GraphicsState = GraphicsState
  { gsUserUnit       :: !Double
  , gsCTM            :: !TransformationMatrix
  , gsScaleX         :: !Double
  , gsScaleY         :: !Double
  , gsTextState      :: !TextState
  , gsLineWidth      :: !Double
  , gsLineCap        :: !Double
  , gsLineJoin       :: !Double
  , gsMiterLimit     :: !Double
  , gsDashArray      :: ![Double]
  , gsDashPhase      :: !Double
  , gsIntent         :: !ByteString
  , gsFlatness       :: !Double
  , gsStrokeAlpha    :: !Double
  , gsNonStrokeAlpha :: !Double
  , gsStrokeColor    :: !Color
  , gsNonStrokeColor :: !Color
  , gsPathStartX     :: !Double
  , gsPathStartY     :: !Double
  , gsCurrentPointX  :: !Double
  , gsCurrentPointY  :: !Double
  } deriving stock (Eq, Show)

{-|
Initial graphics state for a fresh page/content stream.

This corresponds to the default values used before any graphics operators are
applied (identity CTM, default text state, opaque painting, etc.).
-}
defaultGraphicsState :: GraphicsState
defaultGraphicsState = GraphicsState
  { gsUserUnit       = 1.0
  , gsCTM            = mempty
  , gsScaleX         = 1.0
  , gsScaleY         = 1.0
  , gsTextState      = defaultTextState
  , gsLineWidth      = 1.0
  , gsLineCap        = 0.0
  , gsLineJoin       = 0.0
  , gsMiterLimit     = 10.0
  , gsDashArray      = []
  , gsDashPhase      = 0.0
  , gsIntent         = "RelativeColorimetric"
  , gsFlatness       = 1.0
  , gsStrokeAlpha    = 1.0
  , gsNonStrokeAlpha = 1.0
  , gsStrokeColor    = ColorNotSet
  , gsNonStrokeColor = ColorNotSet
  , gsPathStartX     = 0.0
  , gsPathStartY     = 0.0
  , gsCurrentPointX  = 0.0
  , gsCurrentPointY  = 0.0
  }

{-|
Heuristic precision for matrix coefficients at a given scale.

The result is a number of decimal places that are typically meaningful for
rendering/output after taking scaling into account.

Precondition: the scale should be strictly positive.
-}
usefulMatrixPrecisionFor :: Double -> Int
usefulMatrixPrecisionFor scale = max 0 (6 - floor (logBase 10 (abs scale)))

{-|
Heuristic precision for coordinates in the current graphics state.

This is based on the maximum absolute scale implied by the CTM and the user
unit.
-}
usefulGraphicsPrecision :: GraphicsState -> Int
usefulGraphicsPrecision state =
  if scale < 1e-6
    then 0
    else max 0 (ceiling (logBase 10 scale) + 2)
 where
  userUnit = gsUserUnit state
  scaleX   = userUnit * abs (gsScaleX state)
  scaleY   = userUnit * abs (gsScaleY state)
  scale    = max scaleX scaleY

{-|
Heuristic precision for text positioning in the current graphics state.

This takes the CTM scale, user unit, and the text state's rendering scale into
account.
-}
usefulTextPrecision :: GraphicsState -> Int
usefulTextPrecision state =
  if scale < 1e-6
    then 0
    else max 0 (ceiling (logBase 10 scale) + 3)
 where
  userUnit = gsUserUnit state
  scaleTX  = abs ((tsScaleX . gsTextState) state)
  scaleTY  = abs ((tsScaleY . gsTextState) state)
  scaleGX  = abs (gsScaleX state)
  scaleGY  = abs (gsScaleY state)
  scaleX   = userUnit * scaleTX * scaleGX
  scaleY   = userUnit * scaleTY * scaleGY
  scale    = max scaleX scaleY

{-|
Heuristic precision for color components.

This is intentionally a small, fixed value (PDF color components are usually
specified with limited precision in practice).
-}
usefulColorPrecision :: GraphicsState -> Int
usefulColorPrecision _state = 2

{-|
Post-multiply the current transformation matrix (CTM).

This updates both @gsCTM@ and the derived scaling fields (@gsScaleX@, @gsScaleY@)
based on the resulting CTM.
-}
applyGraphicsMatrix :: TransformationMatrix -> GraphicsState -> GraphicsState
applyGraphicsMatrix matrix state = state
  { gsCTM    = graphicsMatrix
  , gsScaleX = scaleX
  , gsScaleY = scaleY
  }
 where
  graphicsMatrix   = gsCTM state <> matrix
  (scaleX, scaleY) = matrixScale graphicsMatrix (1.0, 1.0)

{-|
Post-multiply the current text matrix.

This updates the text state's matrix and its derived scale fields.
-}
applyTextMatrix :: TransformationMatrix -> GraphicsState -> GraphicsState
applyTextMatrix matrix state = state
  { gsTextState = (gsTextState state)
      { tsMatrix = textMatrix
      , tsScaleX = scaleX
      , tsScaleY = scaleY
      }
  }
 where
  textMatrix = tsMatrix (gsTextState state) <> matrix
  (scaleX, scaleY) = matrixScale textMatrix (1.0, 1.0)

{-|
Replace the text matrix with the given matrix.

This differs from 'applyTextMatrix': it does not compose with the existing text
matrix; it overwrites it.

The derived text scale (@tsScaleX@, @tsScaleY@) is computed from the
"text rendering matrix" (text matrix combined with font size, horizontal
scaling, and rise).
-}
setTextMatrix :: TransformationMatrix -> GraphicsState -> GraphicsState
setTextMatrix matrix state = state
  { gsTextState = (gsTextState state)
      { tsMatrix = textMatrix
      , tsScaleX = scaleX
      , tsScaleY = scaleY
      }
  }
 where
  tsState = gsTextState state
  textMatrix = matrix
  renderingMatrix = textMatrix <> (TransformationMatrix
    { tmA = tsFontSize tsState * tsHorizontalScaling tsState
    , tmB = 0.0
    , tmC = 0.0
    , tmD = tsFontSize tsState
    , tmE = 0.0
    , tmF = tsRise tsState
    })
  (scaleX, scaleY) = matrixScale renderingMatrix (1.0, 1.0)

{-|
Set the font and font size of the current text state.

The font name is the PDF font resource name. The size is in text space units
(typically points).
-}
setFont :: ByteString -> Double -> GraphicsState -> GraphicsState
setFont fontName fontSize state = state
  { gsTextState = (gsTextState state)
      { tsFont = fontName
      , tsFontSize = fontSize
      }
  }

{-|
Set the horizontal scaling of the current text state.

The scaling is a double that represents the horizontal scaling factor in
percent (where @100@ means 1.0).
-}
setHorizontalScaling :: Double -> GraphicsState -> GraphicsState
setHorizontalScaling scaling state = state
  { gsTextState = (gsTextState state) { tsHorizontalScaling = scaling / 100 } }

{-|
Set the rise of the current text state.

Rise is the distance, in text space units, to move the baseline up or down.
-}
setTextRise :: Double -> GraphicsState -> GraphicsState
setTextRise rise state = state
  { gsTextState = (gsTextState state) { tsRise = rise } }

{-|
Set the leading of the current text state.

Leading is the distance, in text space units, between baselines.
-}
setTextLeading :: Double -> GraphicsState -> GraphicsState
setTextLeading leading state = state
  { gsTextState = (gsTextState state) { tsLeading = leading } }

{-|
Set the character spacing of the current text state.

Character spacing is added between glyphs when showing text.
-}
setCharacterSpacing :: Double -> GraphicsState -> GraphicsState
setCharacterSpacing spacing state = state
  { gsTextState = (gsTextState state) { tsCharacterSpacing = spacing } }

{-|
Set the word spacing of the current text state.

Word spacing is added to space characters when showing text.
-}
setWordSpacing :: Double -> GraphicsState -> GraphicsState
setWordSpacing spacing state = state
  { gsTextState = (gsTextState state) { tsWordSpacing = spacing } }

{-|
Set the line width of the current graphics state.
-}
setLineWidth :: Double -> GraphicsState -> GraphicsState
setLineWidth lineWidth state = state { gsLineWidth = lineWidth }

{-|
Set the line cap of the current graphics state.
-}
setLineCap :: Double -> GraphicsState -> GraphicsState
setLineCap lineCap state = state { gsLineCap = lineCap }

{-|
Set the line join of the current graphics state.
-}
setLineJoin :: Double -> GraphicsState -> GraphicsState
setLineJoin lineJoin state = state { gsLineJoin = lineJoin }

{-|
Set the miter limit of the current graphics state.
-}
setMiterLimit :: Double -> GraphicsState -> GraphicsState
setMiterLimit miterLimit state = state { gsMiterLimit = miterLimit }

{-|
Set the dash pattern of the current graphics state.

The dash phase is the initial offset into the dash array.
-}
setDashPattern :: Double -> [Double] -> GraphicsState -> GraphicsState
setDashPattern dashPhase dashArray state = state
  { gsDashArray = dashArray
  , gsDashPhase = dashPhase
  }

{-|
Set the rendering intent of the current graphics state.
-}
setRenderingIntent :: ByteString -> GraphicsState -> GraphicsState
setRenderingIntent intent state = state { gsIntent = intent }

{-|
Set the flatness of the current graphics state.
-}
setFlatness :: Double -> GraphicsState -> GraphicsState
setFlatness flatness state = state { gsFlatness = flatness }

{-|
Set the stroke alpha of the current graphics state.
-}
setStrokeAlpha :: Double -> GraphicsState -> GraphicsState
setStrokeAlpha alpha state = state { gsStrokeAlpha = alpha }

{-|
Set the non-stroke alpha of the current graphics state.
-}
setNonStrokeAlpha :: Double -> GraphicsState -> GraphicsState
setNonStrokeAlpha alpha state = state { gsNonStrokeAlpha = alpha }

{-|
Set the start of the current path.

This is used to support path-closing semantics.
-}
setPathStart :: Double -> Double -> GraphicsState -> GraphicsState
setPathStart x y state = state { gsPathStartX = x, gsPathStartY = y }

{-|
Set the current point of the current path.
-}
setCurrentPoint :: Double -> Double -> GraphicsState -> GraphicsState
setCurrentPoint x y state = state { gsCurrentPointX = x, gsCurrentPointY = y }

{-|
Reset the text state matrix to the identity.

This is a convenience wrapper used when beginning/restarting a text object.
-}
resetTextState :: GraphicsState -> GraphicsState
resetTextState = setTextMatrix mempty

{-|
Set the current stroke color in the graphics state.
-}
setStrokeColor :: Color -> GraphicsState -> GraphicsState
setStrokeColor color state = state { gsStrokeColor = color }

{-|
Set the current non-stroke color in the graphics state.
-}
setNonStrokeColor :: Color -> GraphicsState -> GraphicsState
setNonStrokeColor color state = state { gsNonStrokeColor = color }
