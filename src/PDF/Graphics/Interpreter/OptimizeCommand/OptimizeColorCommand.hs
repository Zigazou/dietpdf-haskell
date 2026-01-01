module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeColorCommand
  ( optimizeColorCommand
  ) where

import Control.Monad.State (State, gets)

import Data.PDF.Command (Command (cOperator, cParameters))
import Data.PDF.GFXObject
  ( GFXObject (GFXName)
  , GSOperator (GSSetColourRenderingIntent, GSSetNonStrokeCMYKColorspace, GSSetNonStrokeColor, GSSetNonStrokeColorN, GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetStrokeCMYKColorspace, GSSetStrokeColor, GSSetStrokeColorN, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace)
  )
import Data.PDF.GraphicsState
  (GraphicsState (gsIntent, gsStrokeColor), gsNonStrokeColor)
import Data.PDF.InterpreterAction
  (InterpreterAction (DeleteCommand, KeepCommand), replaceCommandWith)
import Data.PDF.InterpreterState
  ( InterpreterState (iGraphicsState)
  , setNonStrokeColorS
  , setRenderingIntentS
  , setStrokeColorS
  )
import Data.PDF.Program (Program)
import Data.Sequence (Seq (Empty, (:<|)))

import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeColor
  (mkColor, mkNonStrokeCommand, mkStrokeCommand, optimizeColor)

strokeDeleteIfNoChange
  :: Command
  -> State InterpreterState InterpreterAction
strokeDeleteIfNoChange command = do
    currentColor <- gets (gsStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setStrokeColorS newColor
    return $ if currentColor == newColor
              then DeleteCommand
              else replaceCommandWith
                      command
                      (optimizeColor $ mkStrokeCommand newColor)

nonStrokeDeleteIfNoChange
  :: Command
  -> State InterpreterState InterpreterAction
nonStrokeDeleteIfNoChange command = do
    currentColor <- gets (gsNonStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setNonStrokeColorS newColor
    return $ if currentColor == newColor
              then DeleteCommand
              else replaceCommandWith
                      command
                      (optimizeColor $ mkNonStrokeCommand newColor)

{-|
The 'optimizeColorCommand' function takes a 'GraphicsState' and a 'Command' and
returns an optimized 'Command'.
-}
optimizeColorCommand
  :: Command
  -> Program
  -> State InterpreterState InterpreterAction
optimizeColorCommand command _rest = case (operator, parameters) of
  (GSSetColourRenderingIntent, GFXName intent :<| Empty) -> do
    currentIntent <- gets (gsIntent . iGraphicsState)
    if intent == currentIntent
      then return DeleteCommand
      else do
        setRenderingIntentS intent
        return KeepCommand

  (GSSetStrokeColor, _params)          -> strokeDeleteIfNoChange command
  (GSSetStrokeColorN, _params)         -> strokeDeleteIfNoChange command
  (GSSetStrokeGrayColorspace, _params) -> strokeDeleteIfNoChange command
  (GSSetStrokeRGBColorspace, _params)  -> strokeDeleteIfNoChange command
  (GSSetStrokeCMYKColorspace, _params) -> strokeDeleteIfNoChange command

  (GSSetNonStrokeColor, _params)          -> nonStrokeDeleteIfNoChange command
  (GSSetNonStrokeColorN, _params)         -> nonStrokeDeleteIfNoChange command
  (GSSetNonStrokeGrayColorspace, _params) -> nonStrokeDeleteIfNoChange command
  (GSSetNonStrokeRGBColorspace, _params)  -> nonStrokeDeleteIfNoChange command
  (GSSetNonStrokeCMYKColorspace, _params) -> nonStrokeDeleteIfNoChange command

  _anyOtherCommand -> return KeepCommand
 where
  operator   = cOperator command
  parameters = cParameters command
