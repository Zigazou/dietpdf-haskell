{-|
Optimize text matrix transformation commands in PDF streams.

Provides utilities for simplifying text transformation matrices by eliminating
identity matrices, merging consecutive transformations, and converting between
matrix forms.
-}
module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeTextMatrix
  ( optimizeTextMatrix
  ) where

import Control.Monad.State (State, gets)

import Data.Functor ((<&>))
import Data.PDF.Command (Command (Command, cOperator, cParameters))
import Data.PDF.GFXObject
  ( GFXObject (GFXNumber)
  , GSOperator (GSBeginText, GSEndText, GSMoveToNextLine, GSMoveToNextLineLP, GSNextLine, GSSetTextLeading, GSSetTextMatrix, GSNLShowText)
  )
import Data.PDF.GraphicsState (GraphicsState (gsTextState))
import Data.PDF.InterpreterAction
  (InterpreterAction (DeleteCommand, KeepCommand), replaceCommandWith)
import Data.PDF.InterpreterState
  ( InterpreterState (iGraphicsState)
  , applyTextMatrixS
  , resetTextStateS
  , setTextLeadingS
  , setTextMatrixS
  , usefulTextPrecisionS
  )
import Data.PDF.Program (Program)
import Data.PDF.TextState (TextState (tsLeading, tsMatrix))
import Data.PDF.TransformationMatrix
  (TransformationMatrix (TransformationMatrix, tmA, tmB, tmC, tmE, tmF), tmD)
import Data.Sequence (Seq (Empty, (:<|)))

import PDF.Graphics.Interpreter.OptimizeParameters (optimizeParameters)

{-|
Optimize text positioning and transformation matrix commands.

Implements multiple optimization strategies:

* __Begin/End Text__: Resets or tracks text state
* __MoveToNextLine__: Updates text matrix and optimizes precision
* __NextLine__: Uses stored text leading to compute vertical movement
* __SetTextLeading__: Records leading value; used by NextLine operations
* __MoveToNextLineLP__: Sets leading and applies translation in one operation
* __SetTextMatrix__: Detects and removes identity matrices; converts diagonal
  matrices to scale+translate form; converts back to MoveToNextLine when only
  translation differs from current state
* __NLShowText__: Applies leading-based vertical translation

Updates text state with applied transformations and reduces precision as needed.
-}
optimizeTextMatrix
  :: Command
  -> Program
  -> State InterpreterState InterpreterAction
optimizeTextMatrix command _rest = case (operator, parameters) of
  -- Begin text object
  (GSBeginText, Empty) -> do
    resetTextStateS
    return KeepCommand

  -- End text object
  (GSEndText, Empty) -> return KeepCommand

  -- Move to next line
  (GSMoveToNextLine, GFXNumber tx :<| GFXNumber ty :<| Empty) -> do
    precision <- usefulTextPrecisionS
    applyTextMatrixS (TransformationMatrix 1 0 0 1 tx ty)
    return $ replaceCommandWith command
                                (optimizeParameters command precision)

  -- Next line.
  (GSNextLine, Empty) -> do
    textLeading <- gets (tsLeading . gsTextState . iGraphicsState)
    applyTextMatrixS (TransformationMatrix 1 0 0 1 0 (- textLeading))
    return KeepCommand

  -- Set text leading.
  (GSSetTextLeading, GFXNumber leading :<| Empty) -> do
    setTextLeadingS leading
    replaceCommandWith command . optimizeParameters command <$> usefulTextPrecisionS

  -- Move to next line with leading parameter.
  (GSMoveToNextLineLP, GFXNumber tx :<| GFXNumber ty :<| Empty) -> do
    setTextLeadingS (- ty)
    precision <- usefulTextPrecisionS
    applyTextMatrixS (TransformationMatrix 1 0 0 1 tx ty)
    return $ replaceCommandWith command
                                (optimizeParameters command precision)
  -- Set text matrix.
  (GSSetTextMatrix, GFXNumber a
                :<| GFXNumber b
                :<| GFXNumber c
                :<| GFXNumber d
                :<| GFXNumber e
                :<| GFXNumber f
                :<| Empty) -> do
    currentTextMatrix <- gets (tsMatrix . gsTextState . iGraphicsState)
    precision <- usefulTextPrecisionS
    let newTextMatrix = TransformationMatrix
          { tmA = a
          , tmB = b
          , tmC = c
          , tmD = d
          , tmE = e
          , tmF = f
          }
    setTextMatrixS newTextMatrix
    if newTextMatrix == currentTextMatrix
      then
        -- Ignore text transformation matrix if it is identical to the current
        -- one.
        return DeleteCommand
      else case (a, b, c, d, e, f) of
        (newScaleX, 0, 0, newScaleY, newTranslateX, newTranslateY) -> do
          let currentScaleX     = tmA currentTextMatrix
              currentScaleY     = tmD currentTextMatrix
              currentTranslateX = tmE currentTextMatrix
              currentTranslateY = tmF currentTextMatrix

          if newScaleX /= currentScaleX || newScaleY /= currentScaleY
            then return $ replaceCommandWith
                    command
                    ( optimizeParameters
                      (Command GSSetTextMatrix ( GFXNumber newScaleX
                                              :<| GFXNumber 0
                                              :<| GFXNumber 0
                                              :<| GFXNumber newScaleY
                                              :<| GFXNumber newTranslateX
                                              :<| GFXNumber newTranslateY
                                              :<| Empty)
                      )
                      precision
                    )
            else return $ replaceCommandWith
                    command
                    ( optimizeParameters
                      (Command GSMoveToNextLine ( GFXNumber ((newTranslateX - currentTranslateX) / currentScaleX)
                                              :<| GFXNumber ((newTranslateY - currentTranslateY) / currentScaleY)
                                              :<| Empty)
                      )
                      precision
                    )
        _anyOtherTextMatrix -> do
            optimizeParameters command
              <$> usefulTextPrecisionS
              <&> replaceCommandWith command

  -- Show text with new line.
  (GSNLShowText, _anyParameters) -> do
    textLeading <- gets (tsLeading . gsTextState . iGraphicsState)
    applyTextMatrixS (TransformationMatrix 1 0 0 1 0 textLeading)
    return KeepCommand

  _anyOtherCommand -> return KeepCommand
 where
  operator   = cOperator command
  parameters = cParameters command
