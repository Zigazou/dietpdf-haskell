{-|
Optimize PDF graphics color commands.

Provides utilities for simplifying color specifications in graphics streams,
including converting RGB/CMYK to grayscale when appropriate, and bidirectional
conversion between color commands and typed color representations.
-}
module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeColor
  ( optimizeColor
  , mkColor
  , mkStrokeCommand
  , mkNonStrokeCommand
  ) where

import Control.Monad.State (State)

import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.PDF.Color
  (Color (ColorCMYK, ColorGeneric, ColorGray, ColorNotSet, ColorRGB))
import Data.PDF.Command (Command (Command, cOperator, cParameters))
import Data.PDF.GFXObject
  ( GFXObject (GFXName, GFXNull, GFXNumber)
  , GSOperator (GSNone, GSSetNonStrokeCMYKColorspace, GSSetNonStrokeColor, GSSetNonStrokeColorN, GSSetNonStrokeColorspace, GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetStrokeCMYKColorspace, GSSetStrokeColor, GSSetStrokeColorN, GSSetStrokeColorspace, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace)
  )
import Data.PDF.GFXObjects (GFXObjects)
import Data.PDF.InterpreterState (InterpreterState, usefulColorPrecisionS)
import Data.Sequence (Seq (Empty, (:<|), (:|>)), fromList)

import PDF.Graphics.Interpreter.OptimizeParameters (optimizeParameters)


{-|
Optimize a color-setting graphics command.

If the command sets an RGB or CMYK color with equal components (representing
grayscale), converts it to use the more efficient grayscale color space.
Converts RGB(r,r,r) to Grayscale(r) and CMYK(0,0,0,k) to Grayscale(1-k).
Otherwise returns the command unchanged.
-}
optimizeColor :: Command -> Command
optimizeColor command
  | isGray (cParameters command) = case cOperator command of
      GSSetStrokeRGBColorspace ->
        command { cOperator = GSSetStrokeGrayColorspace
                , cParameters = grayLevel parameters :<| Empty
                }
      GSSetNonStrokeRGBColorspace ->
        command { cOperator = GSSetNonStrokeGrayColorspace
                , cParameters = grayLevel parameters :<| Empty
                }
      GSSetStrokeCMYKColorspace ->
        command { cOperator = GSSetStrokeGrayColorspace
                , cParameters = grayLevel parameters :<| Empty
                }
      GSSetNonStrokeCMYKColorspace ->
        command { cOperator = GSSetNonStrokeGrayColorspace
                , cParameters = grayLevel parameters :<| Empty
                }
      _anyOtherOperator -> command
  | otherwise = command
  where
    parameters :: GFXObjects
    parameters = cParameters command

    grayLevel :: GFXObjects -> GFXObject
    grayLevel (red
           :<| _green
           :<| _blue
           :<| Empty) = red
    grayLevel (GFXNumber 0
           :<| GFXNumber 0
           :<| GFXNumber 0
           :<| GFXNumber black
           :<| Empty) = GFXNumber (1.0 - black)
    grayLevel _otherParameters = GFXNull

    isGray :: GFXObjects -> Bool
    isGray (a :<| b :<| c :<| Empty) = a == b && b == c
    isGray (GFXNumber 0
        :<| GFXNumber 0
        :<| GFXNumber 0
        :<| _black
        :<| Empty) = True
    isGray _otherParameters = False

{-|
Construct a typed Color from a graphics command.

Parsescolor-setting commands and extracts the color values, optimizing precision
first. Returns the appropriate Color variant (RGB, CMYK, Gray, Generic, or
NotSet) based on the command type and parameters.
-}
mkColor :: Command -> State InterpreterState Color
mkColor command = do
  optimizedCommand' <- usefulColorPrecisionS <&> optimizeParameters command
  mkColor' optimizedCommand'

 where
  mkColor' :: Command -> State InterpreterState Color
  mkColor' (Command GSSetStrokeRGBColorspace (GFXNumber red
                                          :<| GFXNumber green
                                          :<| GFXNumber blue
                                          :<| Empty)) =
    return $ ColorRGB red green blue

  mkColor' (Command GSSetNonStrokeRGBColorspace (GFXNumber red
                                             :<| GFXNumber green
                                             :<| GFXNumber blue
                                             :<| Empty)) =
    return $ ColorRGB red green blue

  mkColor' (Command GSSetStrokeCMYKColorspace (GFXNumber cyan
                                           :<| GFXNumber magenta
                                           :<| GFXNumber yellow
                                           :<| GFXNumber black
                                           :<| Empty)) =
    return $ ColorCMYK cyan magenta yellow black

  mkColor' (Command GSSetNonStrokeCMYKColorspace (GFXNumber cyan
                                              :<| GFXNumber magenta
                                              :<| GFXNumber yellow
                                              :<| GFXNumber black
                                              :<| Empty)) =
    return $ ColorCMYK cyan magenta yellow black

  mkColor' (Command GSSetStrokeGrayColorspace (GFXNumber gray :<| Empty)) =
    return $ ColorGray gray

  mkColor' (Command GSSetNonStrokeGrayColorspace (GFXNumber gray :<| Empty)) =
    return $ ColorGray gray

  mkColor' (Command GSSetStrokeColorspace (parameters :|> GFXName name)) =
    return $ ColorGeneric (onlyDouble (toList parameters)) (Just name)

  mkColor' (Command GSSetNonStrokeColorspace (parameters :|> GFXName name)) =
    return $ ColorGeneric (onlyDouble (toList parameters)) (Just name)

  mkColor' (Command GSSetStrokeColorN (parameters :|> GFXName name)) =
    return $ ColorGeneric (onlyDouble (toList parameters)) (Just name)

  mkColor' (Command GSSetStrokeColorN parameters) =
    return $ ColorGeneric (onlyDouble (toList parameters)) Nothing

  mkColor' (Command GSSetNonStrokeColorN (parameters :|> GFXName name)) =
    return $ ColorGeneric (onlyDouble (toList parameters)) (Just name)

  mkColor' (Command GSSetNonStrokeColorN parameters) =
    return $ ColorGeneric (onlyDouble (toList parameters)) Nothing

  mkColor' (Command GSSetStrokeColor parameters) =
    return $ ColorGeneric (onlyDouble (toList parameters)) Nothing

  mkColor' (Command GSSetNonStrokeColor parameters) =
    return $ ColorGeneric (onlyDouble (toList parameters)) Nothing

  mkColor' _command = error ("TODO: mkColor' " ++ show command)

  onlyDouble :: [GFXObject] -> [Double]
  onlyDouble = map (\case GFXNumber x -> x; _anyOtherObjectType -> 0)

{-|
Construct a stroke color-setting command from a Color value.

Converts a typed Color into the appropriate graphics command for setting stroke
color. Returns GSSetStrokeRGBColorspace, GSSetStrokeCMYKColorspace,
GSSetStrokeGrayColorspace, GSSetStrokeColorN, GSSetStrokeColor, or GSNone
depending on the color type.
-}
mkStrokeCommand :: Color -> Command
mkStrokeCommand (ColorRGB red green blue) =
  Command GSSetStrokeRGBColorspace
          (   GFXNumber red
          :<| GFXNumber green
          :<| GFXNumber blue
          :<| Empty
          )
mkStrokeCommand (ColorCMYK cyan magenta yellow black) =
  Command GSSetStrokeCMYKColorspace
          (   GFXNumber cyan
          :<| GFXNumber magenta
          :<| GFXNumber yellow
          :<| GFXNumber black
          :<| Empty
          )
mkStrokeCommand (ColorGray gray) =
  Command GSSetStrokeGrayColorspace (GFXNumber gray :<| Empty)

mkStrokeCommand (ColorGeneric parameters (Just name)) =
  Command GSSetStrokeColorN (fromList (GFXNumber <$> parameters) :|> GFXName name)

mkStrokeCommand (ColorGeneric parameters Nothing) =
  Command GSSetStrokeColor (fromList (GFXNumber <$> parameters))

mkStrokeCommand ColorNotSet = Command GSNone mempty

{-|
Construct a non-stroke color-setting command from a Color value.

Converts a typed Color into the appropriate graphics command for setting
non-stroke (fill) color. Returns GSSetNonStrokeRGBColorspace,
GSSetNonStrokeCMYKColorspace, GSSetNonStrokeGrayColorspace,
GSSetNonStrokeColorN, GSSetNonStrokeColor, or GSNone depending on the color
type.
-}
mkNonStrokeCommand :: Color -> Command
mkNonStrokeCommand (ColorRGB red green blue) =
  Command GSSetNonStrokeRGBColorspace
          (   GFXNumber red
          :<| GFXNumber green
          :<| GFXNumber blue
          :<| Empty
          )
mkNonStrokeCommand (ColorCMYK cyan magenta yellow black) =
  Command GSSetNonStrokeCMYKColorspace
          (   GFXNumber cyan
          :<| GFXNumber magenta
          :<| GFXNumber yellow
          :<| GFXNumber black
          :<| Empty
          )
mkNonStrokeCommand (ColorGray gray) =
  Command GSSetNonStrokeGrayColorspace (GFXNumber gray :<| Empty)

mkNonStrokeCommand (ColorGeneric parameters (Just name)) =
  Command GSSetNonStrokeColorN (fromList (GFXNumber <$> parameters)
                            :|> GFXName name)

mkNonStrokeCommand (ColorGeneric parameters Nothing) =
  Command GSSetNonStrokeColor (fromList (GFXNumber <$> parameters))

mkNonStrokeCommand ColorNotSet = Command GSNone mempty
