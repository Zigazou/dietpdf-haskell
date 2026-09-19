{-|
Optimize graphics color commands in PDF streams.

Provides utilities for eliminating redundant color-setting commands by tracking
color state and detecting unchanged colors. Also optimizes color representations
to use more efficient color spaces when possible.
-}
module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeColorCommand
  ( optimizeColorCommand
  ) where

import Control.Monad.State (State, gets)

import Data.List (delete)
import Data.PDF.Command (Command (cOperator, cParameters))
import Data.PDF.GFXObject
  ( GFXObject (GFXName)
  , GSOperator (GSSetColourRenderingIntent, GSSetNonStrokeCMYKColorspace, GSSetNonStrokeColor, GSSetNonStrokeColorN, GSSetNonStrokeColorspace, GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetStrokeCMYKColorspace, GSSetStrokeColor, GSSetStrokeColorN, GSSetStrokeColorspace, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace)
  )
import Data.PDF.GraphicsState
  ( GraphicsState (gsIntent, gsStrokeColor, gsUnknownParameters)
  , gsNonStrokeColor
  , setNonStrokeColorSpace
  , setStrokeColorSpace
  )
import Data.PDF.InterpreterAction
  (InterpreterAction (DeleteCommand, KeepCommand), replaceCommandWith)
import Data.PDF.InterpreterState
  ( InterpreterState (iGraphicsState)
  , modifyGraphicsStateS
  , setNonStrokeColorS
  , setRenderingIntentS
  , setStrokeColorS
  )
import Data.PDF.Program (Program)
import Data.Sequence (Seq (Empty, (:<|)))

import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeColor
  (mkColor, mkNonStrokeCommand, mkStrokeCommand, optimizeColor)

{-|
Optimize a stroke color command by eliminating redundancy.

Parses the color command and extracts its color value. If the new color matches
the current stroke color in graphics state, deletes the command. Otherwise,
updates state and replaces the command with an optimized version.
-}
strokeDeleteIfNoChange
  :: Bool
  -> Command
  -> State InterpreterState InterpreterAction
strokeDeleteIfNoChange allowSpaceChange command = do
    currentColor <- gets (gsStrokeColor . iGraphicsState)
    newColor <- mkColor command
    if currentColor == newColor
      then
        return DeleteCommand
      else do
        let normalized = mkStrokeCommand newColor
            compact = optimizeColor normalized
            emitted = if compact /= normalized && allowSpaceChange
                        then compact
                        else normalized

        emittedColor <- mkColor emitted
        setStrokeColorS emittedColor

        return $ replaceCommandWith command emitted

{-|
Optimize a non-stroke (fill) color command by eliminating redundancy.

Parses the color command and extracts its color value. If the new color matches
the current non-stroke color in graphics state, deletes the command. Otherwise,
updates state and replaces the command with an optimized version.
-}
nonStrokeDeleteIfNoChange
  :: Bool
  -> Command
  -> State InterpreterState InterpreterAction
nonStrokeDeleteIfNoChange allowSpaceChange command = do
    currentColor <- gets (gsNonStrokeColor . iGraphicsState)
    newColor <- mkColor command

    if currentColor == newColor
      then
        return DeleteCommand
      else do
        let normalized = mkNonStrokeCommand newColor
            compact = optimizeColor normalized
            emitted = if compact /= normalized && allowSpaceChange
                        then compact
                        else normalized

        emittedColor <- mkColor emitted
        setNonStrokeColorS emittedColor

        return $ replaceCommandWith command emitted

{-|
Optimize a color or rendering intent command.

Optimizes graphics color commands by eliminating redundant settings when the
color does not change, and by simplifying color representations (e.g., RGB to
grayscale). Also tracks rendering intent changes and removes redundant rendering
intent settings. Returns KeepCommand, DeleteCommand, or a replaced optimized
command as appropriate.
-}
optimizeColorCommand
  :: Command
  -> Program
  -> State InterpreterState InterpreterAction
optimizeColorCommand command rest = case (operator, parameters) of
  (GSSetColourRenderingIntent, GFXName intent :<| Empty) -> do
    currentIntent <- gets (gsIntent . iGraphicsState)
    unknown <- gets ( elem GSSetColourRenderingIntent
                    . gsUnknownParameters
                    . iGraphicsState
                    )

    if not unknown && intent == currentIntent
      then
        return DeleteCommand
      else do
        setRenderingIntentS intent
        modifyGraphicsStateS $ \state -> state
          { gsUnknownParameters = delete GSSetColourRenderingIntent
                                         (gsUnknownParameters state)
          }
        return KeepCommand

  -- CS/cs reset the current color, even when selecting the same color space.
  -- Resource color spaces have different defaults, so forget the cached value
  -- rather than treating a later SC/sc setting as redundant.
  (GSSetStrokeColorspace, _params) -> do
    modifyGraphicsStateS (setStrokeColorSpace selectedSpace)
    return KeepCommand

  (GSSetNonStrokeColorspace, _params) -> do
    modifyGraphicsStateS (setNonStrokeColorSpace selectedSpace)
    return KeepCommand

  (GSSetStrokeColor, _params)          -> strokeDeleteIfNoChange
                                            allowStrokeSpaceChange
                                            command
  (GSSetStrokeColorN, _params)         -> strokeDeleteIfNoChange
                                            allowStrokeSpaceChange
                                            command
  (GSSetStrokeGrayColorspace, _params) -> strokeDeleteIfNoChange
                                            allowStrokeSpaceChange
                                            command
  (GSSetStrokeRGBColorspace, _params)  -> strokeDeleteIfNoChange
                                            allowStrokeSpaceChange
                                            command
  (GSSetStrokeCMYKColorspace, _params) -> strokeDeleteIfNoChange
                                            allowStrokeSpaceChange
                                            command

  (GSSetNonStrokeColor, _params)          -> nonStrokeDeleteIfNoChange
                                              allowFillSpaceChange
                                              command
  (GSSetNonStrokeColorN, _params)         -> nonStrokeDeleteIfNoChange
                                              allowFillSpaceChange
                                              command
  (GSSetNonStrokeGrayColorspace, _params) -> nonStrokeDeleteIfNoChange
                                              allowFillSpaceChange
                                              command
  (GSSetNonStrokeRGBColorspace, _params)  -> nonStrokeDeleteIfNoChange
                                              allowFillSpaceChange
                                              command
  (GSSetNonStrokeCMYKColorspace, _params) -> nonStrokeDeleteIfNoChange
                                              allowFillSpaceChange
                                              command

  _anyOtherCommand -> return KeepCommand
 where
  operator   = cOperator command
  parameters = cParameters command

  selectedSpace = case parameters of
    GFXName name :<| Empty -> Just name
    _other                 -> Nothing
  -- An implicit color command observes the selected space. Preserve device
  -- spaces whenever a later command can depend on them, including across q/Q.
  allowStrokeSpaceChange = not $ any
    ((`elem` [GSSetStrokeColor, GSSetStrokeColorN]) . cOperator) rest
  allowFillSpaceChange = not $ any
    ((`elem` [GSSetNonStrokeColor, GSSetNonStrokeColorN]) . cOperator) rest
