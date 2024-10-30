module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeGraphicsMatrix
  ( optimizeGraphicsMatrix
  ) where

import Control.Monad.State (State)

import Data.PDF.Command (Command (Command, cOperator, cParameters))
import Data.PDF.GFXObject (GFXObject (GFXNumber), GSOperator (GSSetCTM))
import Data.PDF.InterpreterAction
  ( InterpreterAction (DeleteCommand, KeepCommand, ReplaceAndDeleteNextCommand)
  , replaceCommandWith
  )
import Data.PDF.InterpreterState
  (InterpreterState, applyGraphicsMatrixS, usefulGraphicsPrecisionS)
import Data.PDF.Program (Program)
import Data.PDF.TransformationMatrix
  (TransformationMatrix (TransformationMatrix, tmA, tmB, tmC, tmD, tmE, tmF))
import Data.Sequence (Seq (Empty, (:<|)))

import PDF.Graphics.Interpreter.OptimizeParameters (optimizeParameters)


{- |
The 'optimizeCommand' function takes a 'GraphicsState' and a 'Command' and
returns an optimized 'Command'.
-}
optimizeGraphicsMatrix
  :: Command
  -> Program
  -> State InterpreterState InterpreterAction
optimizeGraphicsMatrix command rest = case (operator, parameters) of
  -- Identity matrix can be ignored
  (GSSetCTM, GFXNumber 1
         :<| GFXNumber 0
         :<| GFXNumber 0
         :<| GFXNumber 1
         :<| GFXNumber 0
         :<| GFXNumber 0
         :<| Empty) -> do
    return DeleteCommand

  -- Set current transformation matrix
  (GSSetCTM, GFXNumber a
         :<| GFXNumber b
         :<| GFXNumber c
         :<| GFXNumber d
         :<| GFXNumber e
         :<| GFXNumber f
         :<| Empty) -> do
    precision <- usefulGraphicsPrecisionS
    case rest of
      (Command GSSetCTM ( GFXNumber a'
                      :<| GFXNumber b'
                      :<| GFXNumber c'
                      :<| GFXNumber d'
                      :<| GFXNumber e'
                      :<| GFXNumber f'
                      :<| Empty)
                      :<| _tail) -> do
        -- Merge two consecutive SetCTM operators
        let matrix1 = TransformationMatrix a b c d e f
            matrix2 = TransformationMatrix a' b' c' d' e' f'
            matrix  = matrix1 <> matrix2
        applyGraphicsMatrixS matrix
        let command' = Command GSSetCTM (   GFXNumber (tmA matrix)
                                        :<| GFXNumber (tmB matrix)
                                        :<| GFXNumber (tmC matrix)
                                        :<| GFXNumber (tmD matrix)
                                        :<| GFXNumber (tmE matrix)
                                        :<| GFXNumber (tmF matrix)
                                        :<| Empty
                                        )
        return $ ReplaceAndDeleteNextCommand (optimizeParameters command' precision)
      _anythingElse -> do
        applyGraphicsMatrixS (TransformationMatrix a b c d e f)
        return $ replaceCommandWith command
                                    (optimizeParameters command precision)

  _anyOtherCommand -> return KeepCommand
 where
  operator   = cOperator command
  parameters = cParameters command
