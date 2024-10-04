module Pdf.Graphics.Interpreter.GraphicsState
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
  ) where

import Control.Monad.State (MonadState (get), State, gets, put)

import Data.ByteString qualified as BS
import Data.Kind (Type)

import Pdf.Graphics.Interpreter.TextState
    ( TextState (tsFont, tsFontSize, tsHorizontalScaling, tsMatrix, tsRise)
    , defaultTextState
    , tsScaleX
    , tsScaleY
    )
import Pdf.Graphics.Interpreter.TransformationMatrix
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
  { gsUserUnit  :: !Double
  , gsCTM       :: !TransformationMatrix
  , gsScaleX    :: !Double
  , gsScaleY    :: !Double
  , gsTextState :: !TextState
  , gsStack     :: ![GraphicsState]
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
  { gsUserUnit  = 1.0
  , gsCTM       = mempty
  , gsScaleX    = 1.0
  , gsScaleY    = 1.0
  , gsTextState = defaultTextState
  , gsStack     = []
  }

{- |
Calculates the useful precision of the current graphics state. The useful
precision is the number of decimal places that are useful for rendering
purposes.
-}
usefulGraphicsPrecision :: GraphicsState -> Int
usefulGraphicsPrecision state = max 0 (round (logBase 10 scale) + 1)
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
usefulTextPrecision state = max 0 (round (logBase 10 scale) + 2)
 where
  userUnit = gsUserUnit state
  scaleX   = userUnit * abs ((tsScaleX . gsTextState) state)
  scaleY   = userUnit * abs ((tsScaleY . gsTextState) state)
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
