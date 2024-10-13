module PDF.Graphics.Interpreter.GraphicsState
  ( GraphicsState (..)
  , defaultGraphicsState
  , saveState
  , saveStateS
  , restoreState
  , restoreStateS
  , usefulGraphicsPrecision
  , usefulGraphicsPrecisionS
  , usefulTextPrecision
  , usefulTextPrecisionS
  , usefulColorPrecision
  , usefulColorPrecisionS
  , applyGraphicsMatrix
  , applyGraphicsMatrixS
  , applyTextMatrix
  , applyTextMatrixS
  , setFont
  , setFontS
  , setHorizontalScaling
  , setHorizontalScalingS
  , setTextRise
  , setTextRiseS
  , setPathStart
  , setPathStartS
  , setNonStrokeAlpha
  , setNonStrokeAlphaS
  , setStrokeAlpha
  , setStrokeAlphaS
  , setFlatness
  , setFlatnessS
  , setRenderingIntent
  , setRenderingIntentS
  , setDashPattern
  , setDashPatternS
  , setMiterLimit
  , setMiterLimitS
  , setLineJoin
  , setLineJoinS
  , setLineCap
  , setLineCapS
  , setLineWidth
  , setLineWidthS
  , setCurrentPoint
  , setCurrentPointS
  , getPathStartS
  ) where

import Control.Monad.State (MonadState (get), State, gets, put)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)

import PDF.Graphics.Interpreter.TextState
    ( TextState (tsFont, tsFontSize, tsHorizontalScaling, tsMatrix, tsRise)
    , defaultTextState
    , tsScaleX
    , tsScaleY
    )
import PDF.Graphics.Interpreter.TransformationMatrix
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
  , gsStack          :: ![GraphicsState]
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
  , gsStack          = []
  , gsLineWidth      = 1.0
  , gsLineCap        = 0.0
  , gsLineJoin       = 1.0
  , gsMiterLimit     = 10.0
  , gsDashArray      = []
  , gsDashPhase      = 0.0
  , gsIntent         = "RelativeColorimetric"
  , gsFlatness       = 1.0
  , gsStrokeAlpha    = 1.0
  , gsNonStrokeAlpha = 1.0
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

usefulGraphicsPrecisionS :: State GraphicsState Int
usefulGraphicsPrecisionS = gets usefulGraphicsPrecision

{- |
Calculates the useful precision of the current graphics state. The useful
precision is the number of decimal places that are useful for rendering
purposes.
-}
usefulTextPrecision :: GraphicsState -> Int
usefulTextPrecision state = max 0 (ceiling (logBase 10 scale) + 3)
 where
  userUnit = gsUserUnit state
  scaleTX  = abs ((tsScaleX . gsTextState) state)
  scaleTY  = abs ((tsScaleY . gsTextState) state)
  scaleGX  = abs (gsScaleX state)
  scaleGY  = abs (gsScaleY state)
  scaleX   = userUnit * scaleTX * scaleGX
  scaleY   = userUnit * scaleTY * scaleGY
  scale    = max scaleX scaleY

usefulTextPrecisionS :: State GraphicsState Int
usefulTextPrecisionS = gets usefulTextPrecision

{- |
Calculates the useful precision of the current graphics state. The useful
precision is the number of decimal places that are useful for rendering
purposes.
-}
usefulColorPrecision :: GraphicsState -> Int
usefulColorPrecision _state = 2

usefulColorPrecisionS :: State GraphicsState Int
usefulColorPrecisionS = gets usefulColorPrecision

{- |
Saves the current graphics state to the graphics state stack.
-}
saveState :: GraphicsState -> GraphicsState
saveState state = state { gsStack = state : gsStack state}

saveStateS :: State GraphicsState ()
saveStateS = get >>= put . saveState

{- |
Restores the previous graphics state from the graphics state stack.
-}
restoreState :: GraphicsState -> GraphicsState
restoreState state = case gsStack state of
  []                       -> defaultGraphicsState
  (prevState : prevStates) -> prevState { gsStack = prevStates }

restoreStateS :: State GraphicsState ()
restoreStateS = get >>= put . restoreState

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
  tsState          = gsTextState state
  graphicsMatrix   = gsCTM state <> matrix
  renderingMatrix  = graphicsMatrix <> (TransformationMatrix
    { tmA = tsFontSize tsState * tsHorizontalScaling tsState
    , tmB = 0.0
    , tmC = 0.0
    , tmD = tsFontSize tsState
    , tmE = 0.0
    , tmF = tsRise tsState
    })
  (scaleX, scaleY) = matrixScale renderingMatrix
                                 (gsScaleX state, gsScaleY state)

applyGraphicsMatrixS :: TransformationMatrix -> State GraphicsState ()
applyGraphicsMatrixS matrix = get >>= put . applyGraphicsMatrix matrix

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
  textMatrix = gsCTM state <> matrix
  (scaleX, scaleY) = matrixScale textMatrix (gsScaleX state, gsScaleY state)

applyTextMatrixS :: TransformationMatrix -> State GraphicsState ()
applyTextMatrixS matrix = get >>= put . applyTextMatrix matrix

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

setFontS :: BS.ByteString -> Double -> State GraphicsState ()
setFontS fontName fontSize = get >>= put . setFont fontName fontSize

{- |
Set the horizontal scaling of the current text state.

The scaling is a double that represents the horizontal scaling factor in
percentages (0..100).
-}
setHorizontalScaling :: Double -> GraphicsState -> GraphicsState
setHorizontalScaling scaling state = state
  { gsTextState = (gsTextState state) { tsHorizontalScaling = scaling / 100 } }

setHorizontalScalingS :: Double -> State GraphicsState ()
setHorizontalScalingS scaling = get >>= put . setHorizontalScaling scaling

{- |
Set the rise of the current text state.

The rise is a double that represents the distance, in points, to move the
baseline up or down.
-}
setTextRise :: Double -> GraphicsState -> GraphicsState
setTextRise rise state = state
  { gsTextState = (gsTextState state) { tsRise = rise } }

setTextRiseS :: Double -> State GraphicsState ()
setTextRiseS rise = get >>= put . setTextRise rise

{- |
Set the line width of the current graphics state.
-}
setLineWidth :: Double -> GraphicsState -> GraphicsState
setLineWidth lineWidth state = state { gsLineWidth = lineWidth }

setLineWidthS :: Double -> State GraphicsState ()
setLineWidthS lineWidth = get >>= put . setLineWidth lineWidth

{- |
Set the line cap of the current graphics state.
-}
setLineCap :: Double -> GraphicsState -> GraphicsState
setLineCap lineCap state = state { gsLineCap = lineCap }

setLineCapS :: Double -> State GraphicsState ()
setLineCapS lineCap = get >>= put . setLineCap lineCap

{- |
Set the line join of the current graphics state.
-}
setLineJoin :: Double -> GraphicsState -> GraphicsState
setLineJoin lineJoin state = state { gsLineJoin = lineJoin }

setLineJoinS :: Double -> State GraphicsState ()
setLineJoinS lineJoin = get >>= put . setLineJoin lineJoin

{- |
Set the miter limit of the current graphics state.
-}
setMiterLimit :: Double -> GraphicsState -> GraphicsState
setMiterLimit miterLimit state = state { gsMiterLimit = miterLimit }

setMiterLimitS :: Double -> State GraphicsState ()
setMiterLimitS miterLimit = get >>= put . setMiterLimit miterLimit

{- |
Set the dash pattern of the current graphics state.
-}
setDashPattern :: Double -> [Double] -> GraphicsState -> GraphicsState
setDashPattern dashPhase dashArray state = state
  { gsDashArray = dashArray
  , gsDashPhase = dashPhase
  }

setDashPatternS :: Double -> [Double] -> State GraphicsState ()
setDashPatternS dashPhase dashArray =
  get >>= put . setDashPattern dashPhase dashArray

{- |
Set the rendering intent of the current graphics state.
-}
setRenderingIntent :: ByteString -> GraphicsState -> GraphicsState
setRenderingIntent intent state = state { gsIntent = intent }

setRenderingIntentS :: ByteString -> State GraphicsState ()
setRenderingIntentS intent = get >>= put . setRenderingIntent intent

{- |
Set the flatness of the current graphics state.
-}
setFlatness :: Double -> GraphicsState -> GraphicsState
setFlatness flatness state = state { gsFlatness = flatness }

setFlatnessS :: Double -> State GraphicsState ()
setFlatnessS flatness = get >>= put . setFlatness flatness

{- |
Set the stroke alpha of the current graphics state.
-}
setStrokeAlpha :: Double -> GraphicsState -> GraphicsState
setStrokeAlpha alpha state = state { gsStrokeAlpha = alpha }

setStrokeAlphaS :: Double -> State GraphicsState ()
setStrokeAlphaS alpha = get >>= put . setStrokeAlpha alpha

{- |
Set the non-stroke alpha of the current graphics state.
-}
setNonStrokeAlpha :: Double -> GraphicsState -> GraphicsState
setNonStrokeAlpha alpha state = state { gsNonStrokeAlpha = alpha }

setNonStrokeAlphaS :: Double -> State GraphicsState ()
setNonStrokeAlphaS alpha = get >>= put . setNonStrokeAlpha alpha

{- |
Set the start of the current path.
-}
setPathStart :: Double -> Double -> GraphicsState -> GraphicsState
setPathStart x y state = state { gsPathStartX = x, gsPathStartY = y }

setPathStartS :: Double -> Double -> State GraphicsState ()
setPathStartS x y = get >>= put . setPathStart x y >> setCurrentPointS x y

getPathStartS :: State GraphicsState (Double, Double)
getPathStartS = gets $ \state -> (gsPathStartX state, gsPathStartY state)

{- |
Set the current point of the current path.
-}
setCurrentPoint :: Double -> Double -> GraphicsState -> GraphicsState
setCurrentPoint x y state = state { gsCurrentPointX = x, gsCurrentPointY = y }

setCurrentPointS :: Double -> Double -> State GraphicsState ()
setCurrentPointS x y = get >>= put . setCurrentPoint x y
