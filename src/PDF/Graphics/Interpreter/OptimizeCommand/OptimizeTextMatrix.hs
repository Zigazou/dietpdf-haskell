module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeTextMatrix
  ( optimizeTextMatrix
  ) where

import Control.Monad.State (State, gets)

import Data.Functor ((<&>))
import Data.PDF.Command (Command (Command, cOperator, cParameters))
import Data.PDF.GFXObject
  ( GFXObject (GFXNumber)
  , GSOperator (GSBeginText, GSEndText, GSMoveToNextLine, GSMoveToNextLineLP, GSNextLine, GSSetTextLeading, GSSetTextMatrix)
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
  (TransformationMatrix (TransformationMatrix))
import Data.Sequence (Seq (Empty, (:<|)))

import PDF.Graphics.Interpreter.OptimizeParameters (optimizeParameters)

{- |
The 'optimizeCommand' function takes a 'GraphicsState' and a 'Command' and
returns an optimized 'Command'.
-}
optimizeTextMatrix
  :: Command
  -> Program
  -> State InterpreterState InterpreterAction
optimizeTextMatrix command _rest = case (operator, parameters) of
  -- Begin text object
  (GSBeginText, _params) -> do
    resetTextStateS
    setTextMatrixS mempty
    return KeepCommand

  -- End text object
  (GSEndText, _params) -> return KeepCommand

  -- Identity text matrix can be ignored
  (GSSetTextMatrix, GFXNumber 1
                :<| GFXNumber 0
                :<| GFXNumber 0
                :<| GFXNumber 1
                :<| GFXNumber 0
                :<| GFXNumber 0
                :<| Empty) ->
    return DeleteCommand

  -- Move to next line
  (GSMoveToNextLine, GFXNumber tx
                 :<| GFXNumber ty
                 :<| Empty) -> do
    precision <- usefulTextPrecisionS
    applyTextMatrixS (TransformationMatrix 1 0 0 1 tx ty)
    return $ replaceCommandWith command
                                (optimizeParameters command precision)

  (GSNextLine, _noArgument) -> do
    textLeading <- gets (tsLeading . gsTextState . iGraphicsState)
    applyTextMatrixS (TransformationMatrix 1 0 0 1 0 textLeading)
    return KeepCommand

  (GSSetTextLeading, GFXNumber leading
                  :<| Empty) -> do
    setTextLeadingS leading
    replaceCommandWith command . optimizeParameters command <$> usefulTextPrecisionS

  -- Move to next line LP
  (GSMoveToNextLineLP, GFXNumber tx
                 :<| GFXNumber ty
                 :<| Empty) -> do
    setTextLeadingS ty
    precision <- usefulTextPrecisionS
    applyTextMatrixS (TransformationMatrix 1 0 0 1 tx ty)
    return $ replaceCommandWith command
                                (optimizeParameters command precision)

  -- Set text transformation matrix
  (GSSetTextMatrix, GFXNumber scaleX
                :<| GFXNumber 0
                :<| GFXNumber 0
                :<| GFXNumber scaleY
                :<| GFXNumber translateX
                :<| GFXNumber translateY
                :<| Empty) -> do
    precision <- usefulTextPrecisionS
    (TransformationMatrix currentScaleX _b _c currentScaleY currentTranslateX currentTranslateY) <-
      gets (tsMatrix . gsTextState . iGraphicsState)
    setTextMatrixS (TransformationMatrix scaleX 0 0 scaleY translateX translateY)
    (TransformationMatrix newScaleX _b _c newScaleY newTranslateX newTranslateY) <-
      gets (tsMatrix . gsTextState . iGraphicsState)
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

  (GSSetTextMatrix, GFXNumber a
                :<| GFXNumber b
                :<| GFXNumber c
                :<| GFXNumber d
                :<| GFXNumber e
                :<| GFXNumber f
                :<| Empty) -> do
    setTextMatrixS (TransformationMatrix a b c d e f)
    optimizeParameters command
      <$> usefulTextPrecisionS
      <&> replaceCommandWith command

  _anyOtherCommand -> return KeepCommand
 where
  operator   = cOperator command
  parameters = cParameters command
