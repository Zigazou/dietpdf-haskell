module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeTextCommand
  ( optimizeTextCommand
  ) where

import Control.Monad.State (State)
import Control.Monad.Trans.State (gets)

import Data.Functor ((<&>))
import Data.PDF.Command (Command (Command, cOperator, cParameters))
import Data.PDF.GFXObject
  ( GFXObject (GFXArray, GFXHexString, GFXName, GFXNumber, GFXString)
  , GSOperator (GSBeginText, GSEndText, GSRestoreGS, GSSaveGS, GSSetCharacterSpacing, GSSetHorizontalScaling, GSSetTextFont, GSSetTextRise, GSSetWordSpacing, GSShowManyText, GSShowText)
  )
import Data.PDF.GraphicsState (gsTextState)
import Data.PDF.InterpreterAction
  (InterpreterAction (DeleteCommand, KeepCommand), replaceCommandWith)
import Data.PDF.InterpreterState
  ( InterpreterState (iGraphicsState)
  , resetTextStateS
  , restoreStateS
  , saveStateS
  , setCharacterSpacingS
  , setFontS
  , setHorizontalScalingS
  , setTextRiseS
  , setWordSpacingS
  , usefulTextPrecisionS
  )
import Data.PDF.Program (Program)
import Data.PDF.TextState
  ( TextState (tsCharacterSpacing, tsFont, tsFontSize, tsHorizontalScaling, tsRise, tsWordSpacing)
  )
import Data.Sequence (Seq (Empty, (:<|)))

import PDF.Graphics.Interpreter.OptimizeParameters (optimizeParameters)

import Util.Number (round')

{-|
Delete the command if the new value is the same as the current value.
-}
deleteIfNoChange
  :: Command
  -> Double
  -> (TextState -> Double)
  -> (Double -> State InterpreterState ())
  -> State InterpreterState InterpreterAction
deleteIfNoChange command newValue getter setter = do
    newValue' <- usefulTextPrecisionS <&> flip round' newValue
    currentValue <- gets (getter . gsTextState . iGraphicsState)
    if newValue' == currentValue
      then return DeleteCommand
      else do
        setter newValue'
        optimizeParameters command
          <$> usefulTextPrecisionS
          <&> replaceCommandWith command

{-|
The 'optimizeTextCommand' function takes a 'GraphicsState' and a 'Command' and
returns an optimized 'Command'.
-}
optimizeTextCommand
  :: Command
  -> Program
  -> State InterpreterState InterpreterAction
optimizeTextCommand command _rest = case (operator, parameters) of
  -- Save graphics state
  (GSSaveGS, Empty) -> saveStateS >> return KeepCommand

  -- Restore graphics state
  (GSRestoreGS, Empty) -> restoreStateS >> return KeepCommand

  -- Begin text object
  (GSBeginText, Empty) -> resetTextStateS >> return KeepCommand

  -- End text object
  (GSEndText, Empty) -> return KeepCommand

  -- Set text font and size
  (GSSetTextFont, GFXName fontName :<| GFXNumber fontSize :<| Empty) -> do
    currentFontName <- gets (tsFont . gsTextState . iGraphicsState)
    currentFontSize <- gets (tsFontSize . gsTextState . iGraphicsState)
    if currentFontName == fontName && currentFontSize == fontSize
      then
        return DeleteCommand
      else do
        setFontS fontName fontSize
        replaceCommandWith command . optimizeParameters command <$> usefulTextPrecisionS

  -- Remove ShowManyText when there is no text.
  (GSShowManyText, GFXArray Empty :<| Empty) -> return DeleteCommand

  -- Replace ShowManyText by ShowText when there is only one text
  (GSShowManyText, GFXArray items :<| Empty) -> do
    let newCommand = case items of
          str@(GFXString _string :<| Empty)    -> Command GSShowText str
          str@(GFXHexString _string :<| Empty) -> Command GSShowText str
          _otherContent                        -> command

    optimizeParameters newCommand
      <$> usefulTextPrecisionS
      <&> replaceCommandWith command

  -- Remove ShowText when there is no text.
  (GSShowText, GFXString "" :<| Empty) -> return DeleteCommand

  (GSShowText, _parameters) ->
    optimizeParameters command
      <$> usefulTextPrecisionS
      <&> replaceCommandWith command

  -- Set text rise
  (GSSetTextRise, GFXNumber rise :<| Empty) ->
    deleteIfNoChange command rise tsRise setTextRiseS

  -- Set character spacing
  (GSSetCharacterSpacing, GFXNumber newCharacterSpacing :<| Empty) ->
    deleteIfNoChange command newCharacterSpacing tsCharacterSpacing setCharacterSpacingS

  -- Set word spacing
  (GSSetWordSpacing, GFXNumber newWordSpacing :<| Empty) ->
    deleteIfNoChange command newWordSpacing tsWordSpacing setWordSpacingS

  -- Set text horizontal scaling
  (GSSetHorizontalScaling, GFXNumber scaling :<| Empty) ->
    deleteIfNoChange command scaling tsHorizontalScaling setHorizontalScalingS

  _anyOtherCommand -> return KeepCommand
 where
  operator   = cOperator command
  parameters = cParameters command
