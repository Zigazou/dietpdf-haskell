{-|
Interpreter state and State-monad helpers.

This module defines the state carried while interpreting a PDF content stream.

It combines:

* The current 'GraphicsState'.
* A stack of saved graphics states (corresponding to PDF @q@/@Q@ operators).
* Additional interpreter working data ('WorkData').

Most functions are small adapters that lift a pure 'GraphicsState' update into
the 'State' monad over 'InterpreterState'.
-}
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
  , setTextLeadingS
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
  , setCharacterSpacingS
  , setWordSpacingS
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
  , setCharacterSpacing
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
  , setTextLeading
  , setTextMatrix
  , setTextRise
  , setWordSpacing
  , usefulColorPrecision
  , usefulGraphicsPrecision
  , usefulTextPrecision
  )
import Data.PDF.TransformationMatrix (TransformationMatrix)
import Data.PDF.WorkData (WorkData, emptyWorkData)

{-|
State carried while interpreting/rewriting a content stream.

The graphics state stack is used to implement save/restore semantics.
-}
type InterpreterState :: Type
data InterpreterState = InterpreterState
  { iGraphicsState :: !GraphicsState
  , iStack         :: ![GraphicsState]
  , iWorkData      :: !WorkData
  }

{-|
Default interpreter state.

Uses 'defaultGraphicsState', an empty graphics-state stack, and 'emptyWorkData'.
-}
defaultInterpreterState :: InterpreterState
defaultInterpreterState = InterpreterState
  { iGraphicsState = defaultGraphicsState
  , iStack    = []
  , iWorkData = emptyWorkData
  }

{-|
Saves the current graphics state to the graphics state stack.
-}
saveState :: InterpreterState -> InterpreterState
saveState state = state { iStack = iGraphicsState state : iStack state }

{-|
State-monad variant of 'saveState'.
-}
saveStateS :: State InterpreterState ()
saveStateS = get >>= put . saveState

{-|
Restores the previous graphics state from the graphics state stack.
-}
restoreState :: InterpreterState -> InterpreterState
restoreState state = case iStack state of
  []                       -> state { iGraphicsState = defaultGraphicsState }
  (prevState : prevStates) -> state { iGraphicsState = prevState
                                    , iStack         = prevStates
                                    }

{-|
Apply a pure update to the embedded 'GraphicsState'.
-}
modifyGraphicsState
  :: (GraphicsState -> GraphicsState)
  -> InterpreterState
  -> InterpreterState
modifyGraphicsState f state = state { iGraphicsState = f (iGraphicsState state) }

{-|
State-monad variant of 'modifyGraphicsState'.
-}
modifyGraphicsStateS
  :: (GraphicsState -> GraphicsState)
  -> State InterpreterState ()
modifyGraphicsStateS = modify . modifyGraphicsState

{-|
State-monad variant of 'restoreState'.
-}
restoreStateS :: State InterpreterState ()
restoreStateS = get >>= put . restoreState

{-|
State-monad variant of 'usefulGraphicsPrecision'.
-}
usefulGraphicsPrecisionS :: State InterpreterState Int
usefulGraphicsPrecisionS = gets (usefulGraphicsPrecision . iGraphicsState)

{-|
State-monad variant of 'usefulTextPrecision'.
-}
usefulTextPrecisionS :: State InterpreterState Int
usefulTextPrecisionS = gets (usefulTextPrecision . iGraphicsState)

{-|
State-monad variant of 'usefulColorPrecision'.
-}
usefulColorPrecisionS :: State InterpreterState Int
usefulColorPrecisionS = gets (usefulColorPrecision . iGraphicsState)

{-|
State-monad variant of 'applyGraphicsMatrix'.
-}
applyGraphicsMatrixS :: TransformationMatrix -> State InterpreterState ()
applyGraphicsMatrixS = modifyGraphicsStateS . applyGraphicsMatrix

{-|
State-monad variant of 'applyTextMatrix'.
-}
applyTextMatrixS :: TransformationMatrix -> State InterpreterState ()
applyTextMatrixS = modifyGraphicsStateS . applyTextMatrix

{-|
State-monad variant of 'setTextMatrix'.
-}
setTextMatrixS :: TransformationMatrix -> State InterpreterState ()
setTextMatrixS = modifyGraphicsStateS . setTextMatrix

{-|
State-monad variant of 'setFont'.
-}
setFontS :: ByteString -> Double -> State InterpreterState ()
setFontS fontName fontSize = modifyGraphicsStateS (setFont fontName fontSize)

{-|
State-monad variant of 'setHorizontalScaling'.
-}
setHorizontalScalingS :: Double -> State InterpreterState ()
setHorizontalScalingS = modifyGraphicsStateS . setHorizontalScaling

{-|
State-monad variant of 'setTextRise'.
-}
setTextRiseS :: Double -> State InterpreterState ()
setTextRiseS = modifyGraphicsStateS . setTextRise

{-|
State-monad variant of 'setCharacterSpacing'.
-}
setCharacterSpacingS :: Double -> State InterpreterState ()
setCharacterSpacingS = modifyGraphicsStateS . setCharacterSpacing

{-|
State-monad variant of 'setWordSpacing'.
-}
setWordSpacingS :: Double -> State InterpreterState ()
setWordSpacingS = modifyGraphicsStateS . setWordSpacing

{-|
State-monad variant of 'setTextLeading'.
-}
setTextLeadingS :: Double -> State InterpreterState ()
setTextLeadingS = modifyGraphicsStateS . setTextLeading

{-|
State-monad variant of 'setLineWidth'.
-}
setLineWidthS :: Double -> State InterpreterState ()
setLineWidthS = modifyGraphicsStateS . setLineWidth

{-|
State-monad variant of 'setLineCap'.
-}
setLineCapS :: Double -> State InterpreterState ()
setLineCapS = modifyGraphicsStateS . setLineCap

{-|
State-monad variant of 'setLineJoin'.
-}
setLineJoinS :: Double -> State InterpreterState ()
setLineJoinS = modifyGraphicsStateS . setLineJoin

{-|
State-monad variant of 'setMiterLimit'.
-}
setMiterLimitS :: Double -> State InterpreterState ()
setMiterLimitS = modifyGraphicsStateS . setMiterLimit

{-|
State-monad variant of 'setDashPattern'.
-}
setDashPatternS :: Double -> [Double] -> State InterpreterState ()
setDashPatternS dashPhase dashArray =
  modifyGraphicsStateS (setDashPattern dashPhase dashArray)

{-|
State-monad variant of 'setRenderingIntent'.
-}
setRenderingIntentS :: ByteString -> State InterpreterState ()
setRenderingIntentS = modifyGraphicsStateS . setRenderingIntent

{-|
State-monad variant of 'setFlatness'.
-}
setFlatnessS :: Double -> State InterpreterState ()
setFlatnessS = modifyGraphicsStateS . setFlatness

{-|
State-monad variant of 'setStrokeAlpha'.
-}
setStrokeAlphaS :: Double -> State InterpreterState ()
setStrokeAlphaS = modifyGraphicsStateS . setStrokeAlpha

{-|
State-monad variant of 'setNonStrokeAlpha'.
-}
setNonStrokeAlphaS :: Double -> State InterpreterState ()
setNonStrokeAlphaS = modifyGraphicsStateS . setNonStrokeAlpha

{-|
Set the beginning of the current path and also set the current point.

This matches the common pattern where starting a new subpath establishes the
current point.
-}
setPathStartS :: Double -> Double -> State InterpreterState ()
setPathStartS x y = modifyGraphicsStateS (setPathStart x y)
                 >> modifyGraphicsStateS (setCurrentPoint x y)

{-|
Get the stored start point of the current path.
-}
getPathStartS :: State InterpreterState (Double, Double)
getPathStartS = do
  iState <- get
  return ( (gsPathStartX . iGraphicsState) iState
         , (gsPathStartY . iGraphicsState) iState
         )

{-|
State-monad variant of 'setCurrentPoint'.
-}
setCurrentPointS :: Double -> Double -> State InterpreterState ()
setCurrentPointS x y = modifyGraphicsStateS (setCurrentPoint x y)

{-|
State-monad variant of 'resetTextState'.
-}
resetTextStateS :: State InterpreterState ()
resetTextStateS = modifyGraphicsStateS resetTextState

{-|
State-monad variant of 'setStrokeColor'.
-}
setStrokeColorS :: Color -> State InterpreterState ()
setStrokeColorS = modifyGraphicsStateS . setStrokeColor

{-|
State-monad variant of 'setNonStrokeColor'.
-}
setNonStrokeColorS :: Color -> State InterpreterState ()
setNonStrokeColorS = modifyGraphicsStateS . setNonStrokeColor

{-|
Replace the interpreter working data.
-}
setWorkData :: WorkData -> InterpreterState -> InterpreterState
setWorkData workData state = state { iWorkData = workData }
