module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeColorCommand
  ( optimizeColorCommand
  ) where

import Control.Monad.State (State, gets)

import Data.PDF.Command (Command (cOperator, cParameters))
import Data.PDF.GFXObject
    ( GFXObject (GFXName)
    , GSOperator (GSSetColourRenderingIntent, GSSetNonStrokeCMYKColorspace, GSSetNonStrokeColor, GSSetNonStrokeColorN, GSSetNonStrokeColorspace, GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetStrokeCMYKColorspace, GSSetStrokeColor, GSSetStrokeColorN, GSSetStrokeColorspace, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace)
    )
import Data.PDF.GraphicsState
    ( GraphicsState (gsIntent, gsStrokeColor)
    , gsNonStrokeColor
    )
import Data.PDF.InterpreterAction
    ( InterpreterAction (DeleteCommand, KeepCommand, ReplaceCommand)
    )
import Data.PDF.InterpreterState
    ( InterpreterState (iGraphicsState)
    , setNonStrokeColorS
    , setRenderingIntentS
    , setStrokeColorS
    )
import Data.PDF.Program (Program)
import Data.Sequence (Seq (Empty, (:<|)))

import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeColor
    ( mkColor
    , mkNonStrokeCommand
    , mkStrokeCommand
    )


{- |
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

  (GSSetStrokeColorspace, _parameters) -> do
    currentColor <- gets (gsStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkStrokeCommand newColor)

  (GSSetNonStrokeColorspace, _parameters) -> do
    currentColor <- gets (gsNonStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setNonStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkNonStrokeCommand newColor)

  (GSSetStrokeColor, _parameters) -> do
    currentColor <- gets (gsStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkStrokeCommand newColor)

  (GSSetNonStrokeColor, _parameters) -> do
    currentColor <- gets (gsNonStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setNonStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkNonStrokeCommand newColor)

  (GSSetStrokeColorN, _parameters) -> do
    currentColor <- gets (gsStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkStrokeCommand newColor)

  (GSSetNonStrokeColorN, _parameters) -> do
    currentColor <- gets (gsNonStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setNonStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkNonStrokeCommand newColor)

  (GSSetStrokeGrayColorspace, _parameters) -> do
    currentColor <- gets (gsStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkStrokeCommand newColor)

  (GSSetNonStrokeGrayColorspace, _parameters) -> do
    currentColor <- gets (gsNonStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setNonStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkNonStrokeCommand newColor)

  (GSSetStrokeRGBColorspace, _parameters) -> do
    currentColor <- gets (gsStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkStrokeCommand newColor)

  (GSSetNonStrokeRGBColorspace, _parameters) -> do
    currentColor <- gets (gsNonStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setNonStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkNonStrokeCommand newColor)

  (GSSetStrokeCMYKColorspace, _parameters) -> do
    currentColor <- gets (gsStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkStrokeCommand newColor)

  (GSSetNonStrokeCMYKColorspace, _parameters) -> do
    currentColor <- gets (gsNonStrokeColor . iGraphicsState)
    newColor <- mkColor command
    setNonStrokeColorS newColor
    if currentColor == newColor
      then return DeleteCommand
      else return $ ReplaceCommand (mkNonStrokeCommand newColor)

  _anyOtherCommand -> return KeepCommand
 where
  operator   = cOperator command
  parameters = cParameters command
