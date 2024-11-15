module Data.PDF.InterpreterState
  ( InterpreterState (InterpreterState, iGraphicsState, iStack, iWorkData)
  , defaultInterpreterState
  , saveState
  , saveStateS
  , restoreState
  , restoreStateS
  , usefulGraphicsPrecisionS
  , usefulTextPrecisionS
  , usefulColorPrecisionS
  , applyGraphicsMatrixS
  , setTextMatrixS
  , setFontS
  , setHorizontalScalingS
  , setTextRiseS
  , setLineWidthS
  , setLineCapS
  , setLineJoinS
  , setMiterLimitS
  , setDashPatternS
  , setRenderingIntentS
  , setFlatnessS
  , setStrokeAlphaS
  , setNonStrokeAlphaS
  , setPathStartS
  , getPathStartS
  , setCurrentPointS
  , resetTextStateS
  , setNonStrokeColorS
  , setStrokeColorS
  , setWorkData
  , applyTextMatrixS
  ) where

import Control.Monad.RWS (modify)
import Control.Monad.State (State, get, gets, put)

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.PDF.Color (Color)
import Data.PDF.GraphicsState
  ( GraphicsState
  , applyGraphicsMatrix
  , applyTextMatrix
  , defaultGraphicsState
  , gsPathStartX
  , gsPathStartY
  , resetTextState
  , setCurrentPoint
  , setDashPattern
  , setFlatness
  , setFont
  , setHorizontalScaling
  , setLineCap
  , setLineJoin
  , setLineWidth
  , setMiterLimit
  , setNonStrokeAlpha
  , setNonStrokeColor
  , setPathStart
  , setRenderingIntent
  , setStrokeAlpha
  , setStrokeColor
  , setTextMatrix
  , setTextRise
  , usefulColorPrecision
  , usefulGraphicsPrecision
  , usefulTextPrecision
  )
import Data.PDF.TransformationMatrix (TransformationMatrix)
import Data.PDF.WorkData (WorkData, emptyWorkData)

type InterpreterState :: Type
data InterpreterState = InterpreterState
  { iGraphicsState :: !GraphicsState
  , iStack         :: ![GraphicsState]
  , iWorkData      :: !WorkData
  }

defaultInterpreterState :: InterpreterState
defaultInterpreterState = InterpreterState
  { iGraphicsState = defaultGraphicsState
  , iStack    = []
  , iWorkData = emptyWorkData
  }

{- |
Saves the current graphics state to the graphics state stack.
-}
saveState :: InterpreterState -> InterpreterState
saveState state = state { iStack = iGraphicsState state : iStack state }

saveStateS :: State InterpreterState ()
saveStateS = get >>= put . saveState

{- |
Restores the previous graphics state from the graphics state stack.
-}
restoreState :: InterpreterState -> InterpreterState
restoreState state = case iStack state of
  []                       -> state { iGraphicsState = defaultGraphicsState }
  (prevState : prevStates) -> state { iGraphicsState = prevState
                                    , iStack         = prevStates
                                    }

modifyGraphicsState
  :: (GraphicsState -> GraphicsState)
  -> InterpreterState
  -> InterpreterState
modifyGraphicsState f state = state { iGraphicsState = f (iGraphicsState state) }

modifyGraphicsStateS
  :: (GraphicsState -> GraphicsState)
  -> State InterpreterState ()
modifyGraphicsStateS = modify . modifyGraphicsState

restoreStateS :: State InterpreterState ()
restoreStateS = get >>= put . restoreState

usefulGraphicsPrecisionS :: State InterpreterState Int
usefulGraphicsPrecisionS = gets (usefulGraphicsPrecision . iGraphicsState)

usefulTextPrecisionS :: State InterpreterState Int
usefulTextPrecisionS = gets (usefulTextPrecision . iGraphicsState)

usefulColorPrecisionS :: State InterpreterState Int
usefulColorPrecisionS = gets (usefulColorPrecision . iGraphicsState)

applyGraphicsMatrixS :: TransformationMatrix -> State InterpreterState ()
applyGraphicsMatrixS = modifyGraphicsStateS . applyGraphicsMatrix

applyTextMatrixS :: TransformationMatrix -> State InterpreterState ()
applyTextMatrixS = modifyGraphicsStateS . applyTextMatrix

setTextMatrixS :: TransformationMatrix -> State InterpreterState ()
setTextMatrixS = modifyGraphicsStateS . setTextMatrix

setFontS :: ByteString -> Double -> State InterpreterState ()
setFontS fontName fontSize = modifyGraphicsStateS (setFont fontName fontSize)

setHorizontalScalingS :: Double -> State InterpreterState ()
setHorizontalScalingS = modifyGraphicsStateS . setHorizontalScaling

setTextRiseS :: Double -> State InterpreterState ()
setTextRiseS = modifyGraphicsStateS . setTextRise

setLineWidthS :: Double -> State InterpreterState ()
setLineWidthS = modifyGraphicsStateS . setLineWidth

setLineCapS :: Double -> State InterpreterState ()
setLineCapS = modifyGraphicsStateS . setLineCap

setLineJoinS :: Double -> State InterpreterState ()
setLineJoinS = modifyGraphicsStateS . setLineJoin

setMiterLimitS :: Double -> State InterpreterState ()
setMiterLimitS = modifyGraphicsStateS . setMiterLimit

setDashPatternS :: Double -> [Double] -> State InterpreterState ()
setDashPatternS dashPhase dashArray =
  modifyGraphicsStateS (setDashPattern dashPhase dashArray)

setRenderingIntentS :: ByteString -> State InterpreterState ()
setRenderingIntentS = modifyGraphicsStateS . setRenderingIntent

setFlatnessS :: Double -> State InterpreterState ()
setFlatnessS = modifyGraphicsStateS . setFlatness

setStrokeAlphaS :: Double -> State InterpreterState ()
setStrokeAlphaS = modifyGraphicsStateS . setStrokeAlpha

setNonStrokeAlphaS :: Double -> State InterpreterState ()
setNonStrokeAlphaS = modifyGraphicsStateS . setNonStrokeAlpha

setPathStartS :: Double -> Double -> State InterpreterState ()
setPathStartS x y = modifyGraphicsStateS (setPathStart x y)
                 >> modifyGraphicsStateS (setCurrentPoint x y)

getPathStartS :: State InterpreterState (Double, Double)
getPathStartS = do
  iState <- get
  return ( (gsPathStartX . iGraphicsState) iState
         , (gsPathStartY . iGraphicsState) iState
         )

setCurrentPointS :: Double -> Double -> State InterpreterState ()
setCurrentPointS x y = modifyGraphicsStateS (setCurrentPoint x y)

resetTextStateS :: State InterpreterState ()
resetTextStateS = modifyGraphicsStateS resetTextState

setStrokeColorS :: Color -> State InterpreterState ()
setStrokeColorS = modifyGraphicsStateS . setStrokeColor

setNonStrokeColorS :: Color -> State InterpreterState ()
setNonStrokeColorS = modifyGraphicsStateS . setNonStrokeColor

setWorkData :: WorkData -> InterpreterState -> InterpreterState
setWorkData workData state = state { iWorkData = workData }
