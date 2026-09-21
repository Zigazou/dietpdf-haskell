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
  ( InterpreterState (InterpreterState, iGraphicsState, iStack, iWorkData, iRemainingStrokeColorOps, iRemainingNonStrokeColorOps)
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
  , modifyGraphicsStateS
  , setLineCapS
  , setLineJoinS
  , setMiterLimitS
  , setRenderingIntentS
  , setFlatnessS
  , setPathStartS
  , getPathStartS
  , setCurrentPointS
  , resetTextStateS
  , setNonStrokeColorS
  , setStrokeColorS
  , applyTextMatrixS
  , setCharacterSpacingS
  , setWordSpacingS
  , initRemainingColorOpsS
  , consumeColorOpS
  , allowStrokeColorSpaceChangeS
  , allowNonStrokeColorSpaceChangeS
  ) where

import Control.Monad.RWS (modify)
import Control.Monad.State (State, get, gets, put)

import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.PDF.Color (Color)
import Data.PDF.Command (Command (cOperator))
import Data.PDF.GFXObject
  ( GSOperator (GSSetNonStrokeColor, GSSetNonStrokeColorN, GSSetStrokeColor, GSSetStrokeColorN)
  )
import Data.PDF.GraphicsState
  ( GraphicsState
  , applyGraphicsMatrix
  , applyTextMatrix
  , defaultGraphicsState
  , gsTextState
  , gsPathStartX
  , gsPathStartY
  , resetTextState
  , setCharacterSpacing
  , setCurrentPoint
  , setFlatness
  , setFont
  , setHorizontalScaling
  , setLineCap
  , setLineJoin
  , setLineWidth
  , setMiterLimit
  , setNonStrokeColor
  , setPathStart
  , setRenderingIntent
  , setStrokeColor
  , setTextLeading
  , setTextMatrix
  , setTextRise
  , setWordSpacing
  , usefulColorPrecision
  , usefulGraphicsPrecision
  , usefulTextPrecision
  )
import Data.PDF.Program (Program)
import Data.PDF.TextState (TextState (tsMatrix, tsLineMatrix, tsScaleX, tsScaleY))
import Data.PDF.TransformationMatrix (TransformationMatrix)
import Data.PDF.WorkData (WorkData, emptyWorkData)

{-|
State carried while interpreting/rewriting a content stream.

The graphics state stack is used to implement save/restore semantics.
-}
type InterpreterState :: Type
data InterpreterState = InterpreterState
  { iGraphicsState              :: !GraphicsState -- ^ Current graphics state
  , iStack                      :: ![GraphicsState] -- ^ Stack of saved graphics states
  , iWorkData                   :: !WorkData -- ^ Additional interpreter working data
  , iRemainingStrokeColorOps    :: !Int
    -- ^ Count of SC\/SCN commands not yet consumed in the program being
    -- optimized, used to answer "is there a later stroke color command" in
    -- O(1) instead of rescanning the remaining program
  , iRemainingNonStrokeColorOps :: !Int
    -- ^ Same as 'iRemainingStrokeColorOps', for sc\/scn commands
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
  , iRemainingStrokeColorOps = 0
  , iRemainingNonStrokeColorOps = 0
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
  (prevState : prevStates) -> state { iGraphicsState = preserveTextPosition prevState
                                    , iStack         = prevStates
                                    }
 where
  -- Text matrices are text-object state, not part of the q/Q graphics stack.
  current = gsTextState (iGraphicsState state)
  preserveTextPosition saved = saved
    { gsTextState = (gsTextState saved)
        { tsMatrix = tsMatrix current
        , tsLineMatrix = tsLineMatrix current
        , tsScaleX = tsScaleX current
        , tsScaleY = tsScaleY current
        }
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
Whether an operator is a stroke color-value command (SC\/SCN).
-}
isStrokeColorOp :: GSOperator -> Bool
isStrokeColorOp GSSetStrokeColor  = True
isStrokeColorOp GSSetStrokeColorN = True
isStrokeColorOp _anyOtherOperator = False

{-|
Whether an operator is a non-stroke (fill) color-value command (sc\/scn).
-}
isNonStrokeColorOp :: GSOperator -> Bool
isNonStrokeColorOp GSSetNonStrokeColor  = True
isNonStrokeColorOp GSSetNonStrokeColorN = True
isNonStrokeColorOp _anyOtherOperator    = False

{-|
Scan @program@ once and record how many stroke and non-stroke color-value
commands it contains.

Must be called before optimizing a program, so that
'allowStrokeColorSpaceChangeS' and 'allowNonStrokeColorSpaceChangeS' can answer
in O(1) per command instead of rescanning the remaining program for every
color-setting command, which would make color optimization quadratic in the
number of commands.
-}
initRemainingColorOpsS :: Program -> State InterpreterState ()
initRemainingColorOpsS program = modify $ \state -> state
  { iRemainingStrokeColorOps    = countOps isStrokeColorOp
  , iRemainingNonStrokeColorOps = countOps isNonStrokeColorOp
  }
 where
  countOps predicate =
    foldl' (\acc command -> if predicate (cOperator command) then acc + 1 else acc)
           0
           program

{-|
Record that @command@ has left the not-yet-processed part of the program
(kept, deleted or replaced), decrementing the remaining color-op counters when
its operator is one that 'initRemainingColorOpsS' counted.
-}
consumeColorOpS :: Command -> State InterpreterState ()
consumeColorOpS command = modify $ \state -> state
  { iRemainingStrokeColorOps    = iRemainingStrokeColorOps state
      - (if isStrokeColorOp operator then 1 else 0)
  , iRemainingNonStrokeColorOps = iRemainingNonStrokeColorOps state
      - (if isNonStrokeColorOp operator then 1 else 0)
  }
 where operator = cOperator command

{-|
Whether a stroke color-value command other than @command@ itself remains in
the not-yet-processed part of the program.
-}
allowStrokeColorSpaceChangeS :: Command -> State InterpreterState Bool
allowStrokeColorSpaceChangeS command = gets $ \state ->
  iRemainingStrokeColorOps state - (if isStrokeColorOp (cOperator command) then 1 else 0) <= 0

{-|
Whether a non-stroke (fill) color-value command other than @command@ itself
remains in the not-yet-processed part of the program.
-}
allowNonStrokeColorSpaceChangeS :: Command -> State InterpreterState Bool
allowNonStrokeColorSpaceChangeS command = gets $ \state ->
  iRemainingNonStrokeColorOps state - (if isNonStrokeColorOp (cOperator command) then 1 else 0) <= 0
