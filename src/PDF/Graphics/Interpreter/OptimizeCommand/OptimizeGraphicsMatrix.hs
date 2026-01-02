{-|
Optimize graphics transformation matrix commands.

Provides utilities for simplifying PDF graphics state transformation matrix
commands (SetCTM) by eliminating identity matrices and merging consecutive
matrix operations.
-}
module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeGraphicsMatrix
  ( optimizeGraphicsMatrix
  ) where

import Control.Monad.State (State)

import Data.PDF.Command (Command (Command, cOperator, cParameters))
import Data.PDF.GFXObject (GFXObject (GFXNumber), GSOperator (GSSetCTM))
import Data.PDF.GraphicsState (usefulMatrixPrecisionFor)
import Data.PDF.InterpreterAction
  ( InterpreterAction (DeleteCommand, KeepCommand, ReplaceAndDeleteNextCommand)
  , replaceCommandWith
  )
import Data.PDF.InterpreterState (InterpreterState, applyGraphicsMatrixS)
import Data.PDF.Program (Program)
import Data.PDF.TransformationMatrix
  (TransformationMatrix (TransformationMatrix, tmA, tmB, tmC, tmD, tmE, tmF))
import Data.Sequence (Seq (Empty, (:<|)))

import PDF.Graphics.Interpreter.OptimizeParameters (optimizeParameters)


{-|
Optimize a graphics transformation matrix (SetCTM) command.

Implements multiple optimization strategies:

* __Identity matrix__: Deletes SetCTM commands that specify the identity matrix
  [1 0 0 1 0 0], as they have no effect
* __Consecutive SetCTM commands__: Merges two consecutive SetCTM operations by
  computing their matrix product, eliminating the first command
* __Coordinate precision__: Reduces floating-point precision of matrix
  components based on the first component's magnitude

Updates graphics state with the applied transformation matrix.
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
    let precision = usefulMatrixPrecisionFor a
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
