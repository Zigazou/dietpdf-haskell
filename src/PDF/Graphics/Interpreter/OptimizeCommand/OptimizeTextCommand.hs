module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeTextCommand
  ( optimizeTextCommand
  ) where

import Control.Monad.State (State)
import Control.Monad.Trans.State (gets)

import Data.Functor ((<&>))
import Data.PDF.Command (Command (Command, cOperator, cParameters))
import Data.PDF.GFXObject
  ( GFXObject (GFXArray, GFXHexString, GFXName, GFXNumber, GFXString)
  , GSOperator (GSSetHorizontalScaling, GSSetTextFont, GSSetTextRise, GSShowManyText, GSShowText)
  )
import Data.PDF.GraphicsState (gsTextState)
import Data.PDF.InterpreterAction
  (InterpreterAction (DeleteCommand, KeepCommand), replaceCommandWith)
import Data.PDF.InterpreterState
  ( InterpreterState (iGraphicsState)
  , setFontS
  , setHorizontalScalingS
  , setTextRiseS
  , usefulGraphicsPrecisionS
  , usefulTextPrecisionS
  )
import Data.PDF.Program (Program)
import Data.PDF.TextState (TextState (tsFont, tsFontSize, tsHorizontalScaling, tsRise))
import Data.Sequence (Seq (Empty, (:<|)))

import PDF.Graphics.Interpreter.OptimizeParameters (optimizeParameters)

{- |
The 'optimizeTextCommand' function takes a 'GraphicsState' and a 'Command' and
returns an optimized 'Command'.
-}
optimizeTextCommand
  :: Command
  -> Program
  -> State InterpreterState InterpreterAction
optimizeTextCommand command _rest = case (operator, parameters) of
  -- Set text font and size
  (GSSetTextFont, GFXName fontName :<| GFXNumber fontSize :<| Empty) -> do
    currentFontName <- gets (tsFont . gsTextState . iGraphicsState)
    currentFontSize <- gets (tsFontSize . gsTextState . iGraphicsState)
    if currentFontName == fontName && currentFontSize == fontSize
      then
        return DeleteCommand
      else do
        setFontS fontName fontSize
        precision <- usefulTextPrecisionS
        return $ replaceCommandWith command
                                    (optimizeParameters command (precision + 1))

  -- Set text horizontal scaling
  (GSSetHorizontalScaling, GFXNumber scaling :<| Empty) -> do
    currentScaling <- gets (tsHorizontalScaling . gsTextState . iGraphicsState)
    if currentScaling == scaling
      then
        return DeleteCommand
      else do
        setHorizontalScalingS scaling
        optimizeParameters command
          <$> usefulGraphicsPrecisionS
          <&> replaceCommandWith command

  -- Set text rise
  (GSSetTextRise, GFXNumber rise :<| Empty) -> do
    currentRise <- gets (tsRise . gsTextState . iGraphicsState)
    if currentRise == rise
      then
        return DeleteCommand
      else do
        setTextRiseS rise
        optimizeParameters command
          <$> usefulTextPrecisionS
          <&> replaceCommandWith command

  -- Replace ShowManyText by ShowText when there is only one text
  (GSShowManyText, GFXArray items :<| Empty) -> do
    let newCommand = case items of
          str@(GFXString _string :<| Empty)    -> Command GSShowText str
          str@(GFXHexString _string :<| Empty) -> Command GSShowText str
          _otherContent                        -> command

    optimizeParameters newCommand
      <$> usefulTextPrecisionS
      <&> replaceCommandWith command

  (GSShowText, _parameters) ->
    optimizeParameters command
      <$> usefulTextPrecisionS
      <&> replaceCommandWith command

  _anyOtherCommand -> return KeepCommand
 where
  operator   = cOperator command
  parameters = cParameters command
