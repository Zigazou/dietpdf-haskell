module Data.PDF.GraphicsState
  ( GraphicsState (..)
  , defaultGraphicsState
  , usefulGraphicsPrecision
  , usefulTextPrecision
  , usefulColorPrecision
  , applyGraphicsMatrix
  , applyTextMatrix
  , setFont
  , setHorizontalScaling
  , setTextRise
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
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.PDF.Color (Color (ColorGray))
import Data.PDF.TextState
    ( TextState (tsFont, tsFontSize, tsHorizontalScaling, tsMatrix, tsRise)
    , defaultTextState
    , tsScaleX
    , tsScaleY
    )
import Data.PDF.TransformationMatrix
    ( TransformationMatrix (TransformationMatrix, tmA, tmB, tmC, tmD, tmE, tmF)
    , matrixScale
    )

{- |
The graphics state is a collection of parameters that define the current page's
graphics context. The graphics state includes the following parameters:

* The current user unit
* The current transformation matrix (CTM)
* The current text state
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

{- |
The default graphics state is the initial graphics state of a page. It has the
following parameters:

* The current user unit is 1.0
* The current transformation matrix (CTM) is the identity matrix
* The current text state is the default text state
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
  , gsStrokeColor    = ColorGray 0
  , gsNonStrokeColor = ColorGray 0
  , gsPathStartX     = 0.0
  , gsPathStartY     = 0.0
  , gsCurrentPointX  = 0.0
  , gsCurrentPointY  = 0.0
  }

{- |
Calculates the useful precision of the current graphics state. The useful
precision is the number of decimal places that are useful for rendering
purposes.
-}
usefulGraphicsPrecision :: GraphicsState -> Int
usefulGraphicsPrecision state = max 0 (ceiling (logBase 10 scale) + 2)
 where
  userUnit = gsUserUnit state
  scaleX   = userUnit * abs (gsScaleX state)
  scaleY   = userUnit * abs (gsScaleY state)
  scale    = max scaleX scaleY

{- |
Calculates the useful precision of the current graphics state. The useful
precision is the number of decimal places that are useful for rendering
purposes.
-}
usefulTextPrecision :: GraphicsState -> Int
usefulTextPrecision state = max 0 (ceiling (logBase 10 scale) + 2)
 where
  userUnit = gsUserUnit state
  scaleTX  = abs ((tsScaleX . gsTextState) state)
  scaleTY  = abs ((tsScaleY . gsTextState) state)
  scaleGX  = abs (gsScaleX state)
  scaleGY  = abs (gsScaleY state)
  scaleX   = userUnit * scaleTX * scaleGX
  scaleY   = userUnit * scaleTY * scaleGY
  scale    = max scaleX scaleY

{- |
Calculates the useful precision of the current graphics state. The useful
precision is the number of decimal places that are useful for rendering
purposes.
-}
usefulColorPrecision :: GraphicsState -> Int
usefulColorPrecision _state = 2

{- |
Applies a transformation matrix to the current transformation matrix (CTM) of
the graphics state.
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

{- |
Set the text matrix of the current text state.

Contrary to the 'applyGraphicsMatrix' function, this function replaces the
current text matrix with the given matrix. Plus, the resulting text matrix is
the product of the given matrix and the current text matrix.
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
  tsState          = gsTextState state
  textMatrix = gsCTM state <> matrix
  renderingMatrix  = textMatrix <> (TransformationMatrix
    { tmA = tsFontSize tsState * tsHorizontalScaling tsState
    , tmB = 0.0
    , tmC = 0.0
    , tmD = tsFontSize tsState
    , tmE = 0.0
    , tmF = tsRise tsState
    })
  (scaleX, scaleY) = matrixScale renderingMatrix (1.0, 1.0)

{- |
Set the font and font size of the current text state.

The font name is a byte string that represents the font name. The font size is
a double that represents the font size in points.
-}
setFont :: BS.ByteString -> Double -> GraphicsState -> GraphicsState
setFont fontName fontSize state = state
  { gsTextState = (gsTextState state)
      { tsFont = fontName
      , tsFontSize = fontSize
      }
  }

{- |
Set the horizontal scaling of the current text state.

The scaling is a double that represents the horizontal scaling factor in
percentages (0..100).
-}
setHorizontalScaling :: Double -> GraphicsState -> GraphicsState
setHorizontalScaling scaling state = state
  { gsTextState = (gsTextState state) { tsHorizontalScaling = scaling / 100 } }

{- |
Set the rise of the current text state.

The rise is a double that represents the distance, in points, to move the
baseline up or down.
-}
setTextRise :: Double -> GraphicsState -> GraphicsState
setTextRise rise state = state
  { gsTextState = (gsTextState state) { tsRise = rise } }

{- |
Set the line width of the current graphics state.
-}
setLineWidth :: Double -> GraphicsState -> GraphicsState
setLineWidth lineWidth state = state { gsLineWidth = lineWidth }

{- |
Set the line cap of the current graphics state.
-}
setLineCap :: Double -> GraphicsState -> GraphicsState
setLineCap lineCap state = state { gsLineCap = lineCap }

{- |
Set the line join of the current graphics state.
-}
setLineJoin :: Double -> GraphicsState -> GraphicsState
setLineJoin lineJoin state = state { gsLineJoin = lineJoin }

{- |
Set the miter limit of the current graphics state.
-}
setMiterLimit :: Double -> GraphicsState -> GraphicsState
setMiterLimit miterLimit state = state { gsMiterLimit = miterLimit }

{- |
Set the dash pattern of the current graphics state.
-}
setDashPattern :: Double -> [Double] -> GraphicsState -> GraphicsState
setDashPattern dashPhase dashArray state = state
  { gsDashArray = dashArray
  , gsDashPhase = dashPhase
  }

{- |
Set the rendering intent of the current graphics state.
-}
setRenderingIntent :: ByteString -> GraphicsState -> GraphicsState
setRenderingIntent intent state = state { gsIntent = intent }

{- |
Set the flatness of the current graphics state.
-}
setFlatness :: Double -> GraphicsState -> GraphicsState
setFlatness flatness state = state { gsFlatness = flatness }

{- |
Set the stroke alpha of the current graphics state.
-}
setStrokeAlpha :: Double -> GraphicsState -> GraphicsState
setStrokeAlpha alpha state = state { gsStrokeAlpha = alpha }

{- |
Set the non-stroke alpha of the current graphics state.
-}
setNonStrokeAlpha :: Double -> GraphicsState -> GraphicsState
setNonStrokeAlpha alpha state = state { gsNonStrokeAlpha = alpha }

{- |
Set the start of the current path.
-}
setPathStart :: Double -> Double -> GraphicsState -> GraphicsState
setPathStart x y state = state { gsPathStartX = x, gsPathStartY = y }

{- |
Set the current point of the current path.
-}
setCurrentPoint :: Double -> Double -> GraphicsState -> GraphicsState
setCurrentPoint x y state = state { gsCurrentPointX = x, gsCurrentPointY = y }

resetTextState :: GraphicsState -> GraphicsState
resetTextState state = state { gsTextState = defaultTextState }

setStrokeColor :: Color -> GraphicsState -> GraphicsState
setStrokeColor color state = state { gsStrokeColor = color }

setNonStrokeColor :: Color -> GraphicsState -> GraphicsState
setNonStrokeColor color state = state { gsNonStrokeColor = color }
