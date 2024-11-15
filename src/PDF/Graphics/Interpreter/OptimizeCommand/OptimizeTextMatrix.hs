module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeTextMatrix
  ( optimizeTextMatrix
  ) where

import Control.Monad.State (State, gets)

import Data.Functor ((<&>))
import Data.PDF.Command (Command (Command, cOperator, cParameters))
import Data.PDF.GFXObject
  ( GFXObject (GFXNumber)
  , GSOperator (GSBeginText, GSCloseSubpath, GSEndPath, GSEndText, GSMoveToNextLine, GSMoveToNextLineLP, GSSetTextMatrix)
  )
import Data.PDF.GraphicsState (GraphicsState (gsTextState))
import Data.PDF.InterpreterAction
  (InterpreterAction (DeleteCommand, KeepCommand), replaceCommandWith)
import Data.PDF.InterpreterState
  ( InterpreterState (iGraphicsState)
  , applyTextMatrixS
  , resetTextStateS
  , setTextMatrixS
  , usefulTextPrecisionS
  )
import Data.PDF.Program (Program)
import Data.PDF.TextState (TextState (tsMatrix))
import Data.PDF.TransformationMatrix
  (TransformationMatrix (TransformationMatrix))
import Data.Sequence (Seq (Empty, (:<|)))

import PDF.Graphics.Interpreter.OptimizeParameters (optimizeParameters)


hasTextmoveBeforeEndText :: Program -> Bool
hasTextmoveBeforeEndText (Command GSEndPath _params :<| _tail)          = False
hasTextmoveBeforeEndText (Command GSCloseSubpath _params :<| _tail)     = False
hasTextmoveBeforeEndText (Command GSMoveToNextLine _params :<| _tail)   = True
hasTextmoveBeforeEndText (Command GSMoveToNextLineLP _params :<| _tail) = True
hasTextmoveBeforeEndText (Command GSSetTextMatrix _params :<| _tail)    = True
hasTextmoveBeforeEndText Empty                                          = False
hasTextmoveBeforeEndText (_command :<| rest) = hasTextmoveBeforeEndText rest

{- |
The 'optimizeCommand' function takes a 'GraphicsState' and a 'Command' and
returns an optimized 'Command'.
-}
optimizeTextMatrix
  :: Command
  -> Program
  -> State InterpreterState InterpreterAction
optimizeTextMatrix command rest = case (operator, parameters) of
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

  -- Set text transformation matrix
  (GSSetTextMatrix, GFXNumber 1
                :<| GFXNumber 0
                :<| GFXNumber 0
                :<| GFXNumber 1
                :<| GFXNumber tx
                :<| GFXNumber ty
                :<| Empty) -> do
    precision <- usefulTextPrecisionS
    (TransformationMatrix _a _b _c _d currentTx currentTy) <-
      gets (tsMatrix . gsTextState . iGraphicsState)
    setTextMatrixS (TransformationMatrix 1 0 0 1 tx ty)
    (TransformationMatrix _a _b _c _d newTx newTy) <-
      gets (tsMatrix . gsTextState . iGraphicsState)
    return $ replaceCommandWith
              command
              ( optimizeParameters
                (Command GSMoveToNextLine ( GFXNumber (newTx - currentTx)
                                        :<| GFXNumber (newTy - currentTy)
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
