module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeTextMatrix
  ( optimizeTextMatrix
  ) where

import Control.Monad.State (State)

import Data.PDF.Command (Command (Command, cOperator, cParameters))
import Data.PDF.GFXObject
    ( GFXObject (GFXNumber)
    , GSOperator (GSBeginText, GSCloseSubpath, GSEndPath, GSEndText, GSMoveToNextLine, GSMoveToNextLineLP, GSSetTextMatrix)
    )
import Data.PDF.InterpreterAction
    ( InterpreterAction (DeleteCommand, KeepCommand, ReplaceCommand)
    )
import Data.PDF.InterpreterState
    ( InterpreterState
    , applyTextMatrixS
    , resetTextStateS
    , usefulTextPrecisionS
    )
import Data.PDF.Program (Program)
import Data.PDF.TransformationMatrix
    ( TransformationMatrix (TransformationMatrix)
    )
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
    applyTextMatrixS mempty
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

  -- Set text transformation matrix
  (GSSetTextMatrix, GFXNumber 1
                :<| GFXNumber 0
                :<| GFXNumber 0
                :<| GFXNumber 1
                :<| GFXNumber tx
                :<| GFXNumber ty
                :<| Empty) -> do
    precision <- usefulTextPrecisionS
    if hasTextmoveBeforeEndText rest
      then do
        applyTextMatrixS (TransformationMatrix 1 0 0 1 tx ty)
        return $ ReplaceCommand (optimizeParameters command precision)
      else
        return $ ReplaceCommand
                  ( optimizeParameters
                    (Command GSMoveToNextLine ( GFXNumber tx
                                            :<| GFXNumber ty
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
    applyTextMatrixS (TransformationMatrix a b c d e f)
    ReplaceCommand . optimizeParameters command <$> usefulTextPrecisionS

  _anyOtherCommand -> return KeepCommand
 where
  operator   = cOperator command
  parameters = cParameters command
