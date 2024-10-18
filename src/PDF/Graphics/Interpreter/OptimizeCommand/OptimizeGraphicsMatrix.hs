module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeGraphicsMatrix
  ( optimizeGraphicsMatrix
  ) where

import Control.Monad.State (State)

import Data.Functor ((<&>))
import Data.PDF.Command (Command (Command, cOperator, cParameters))
import Data.PDF.GFXObject (GFXObject (GFXNumber), GSOperator (GSSetCTM))
import Data.PDF.InterpreterAction
    ( InterpreterAction (DeleteCommand, KeepCommand, ReplaceAndDeleteNextCommand, ReplaceCommand)
    )
import Data.PDF.InterpreterState
    ( InterpreterState
    , applyGraphicsMatrixS
    , usefulGraphicsPrecisionS
    )
import Data.PDF.Program (Program)
import Data.PDF.TransformationMatrix
    ( TransformationMatrix (TransformationMatrix)
    )
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
    precision <- usefulGraphicsPrecisionS <&> (+ 1)
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
        applyGraphicsMatrixS (TransformationMatrix a b c d e f)
        applyGraphicsMatrixS (TransformationMatrix a' b' c' d' e' f')
        let command' = Command GSSetCTM (   GFXNumber (a * a')
                                        :<| GFXNumber (b * a')
                                        :<| GFXNumber (c * a')
                                        :<| GFXNumber (d * a')
                                        :<| GFXNumber (e * a' + f)
                                        :<| GFXNumber (f * a' + f')
                                        :<| Empty
                                        )
        return $ ReplaceAndDeleteNextCommand (optimizeParameters command' precision)
      _anythingElse -> do
        applyGraphicsMatrixS (TransformationMatrix a b c d e f)
        return $ ReplaceCommand (optimizeParameters command precision)

  _anyOtherCommand -> return KeepCommand
 where
  operator   = cOperator command
  parameters = cParameters command
