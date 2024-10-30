module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeTextCommand
  ( optimizeTextCommand
  ) where

import Control.Monad.State (State)

import Data.Functor ((<&>))
import Data.PDF.Command (Command (Command, cOperator, cParameters))
import Data.PDF.GFXObject
  ( GFXObject (GFXArray, GFXHexString, GFXName, GFXNumber, GFXString)
  , GSOperator (GSSetHorizontalScaling, GSSetTextFont, GSSetTextRise, GSShowManyText, GSShowText)
  )
import Data.PDF.InterpreterAction
  (InterpreterAction (KeepCommand), replaceCommandWith)
import Data.PDF.InterpreterState
  ( InterpreterState
  , setFontS
  , setHorizontalScalingS
  , setTextRiseS
  , usefulGraphicsPrecisionS
  , usefulTextPrecisionS
  )
import Data.PDF.Program (Program)
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
    setFontS fontName fontSize
    precision <- usefulTextPrecisionS
    return $ replaceCommandWith command
                                (optimizeParameters command (precision + 1))

  -- Set text horizontal scaling
  (GSSetHorizontalScaling, GFXNumber scaling :<| Empty) -> do
    setHorizontalScalingS scaling
    optimizeParameters command
      <$> usefulGraphicsPrecisionS
      <&> replaceCommandWith command

  -- Set text rise
  (GSSetTextRise, GFXNumber rise :<| Empty) -> do
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
