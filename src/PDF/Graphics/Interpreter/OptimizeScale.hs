{-|
PDF graphics program scaling optimization.

This module provides functionality to scale PDF graphics programs by multiplying
coordinate parameters and prepending a transformation matrix command.
-}
module PDF.Graphics.Interpreter.OptimizeScale
  ( optimizeScale
  ) where

import Data.PDF.Command (Command (cOperator, cParameters), mkCommand)
import Data.PDF.GFXObject
  ( GFXObject (GFXNumber)
  , GSOperator (GSCubicBezierCurve, GSCubicBezierCurve1To, GSCubicBezierCurve2To, GSLineTo, GSMoveTo, GSMoveToNextLine, GSMoveToNextLineLP, GSRectangle, GSRestoreGS, GSSaveGS, GSSetCTM, GSSetLineWidth, GSSetTextFont, GSSetTextMatrix, GSSetWordSpacing, GSSetCharacterSpacing)
  )
import Data.PDF.Program (Program)
import Data.Sequence (Seq ((:<|)), (<|), (|>))

{-|
Scale a PDF graphics program by a scaling factor.

Prepends a transformation matrix command (cm) that scales down by the inverse of
the scale factor, then multiplies coordinate parameters of relevant commands by
the scale factor. This allows coordinates to be expressed in larger units while
maintaining the same visual output.

__For example__, with @scale = 100.0@:
- Prepends: @0.01 0 0 0.01 0 0 cm@ (scales down by 1/100)
- Multiplies coordinates in commands like MoveTo, LineTo, etc. by 100

__Affected commands (all parameters scaled):__

- Path construction: MoveTo (m), LineTo (l), Rectangle (re)
- Curves: CubicBezierCurve (c), CubicBezierCurve1To (v), CubicBezierCurve2To (y)
- Text positioning: MoveToNextLine (Td), MoveToNextLineLP (TD), SetTextMatrix (Tm)

__Affected commands (specific parameters scaled):__

- Graphics state: SetLineWidth (w) - width parameter only
- Transformation: SetCTM (cm) - translation parameters (e and f) only

__Parameters:__

- @scale@: The scaling factor to apply to coordinates
- @program@: The PDF graphics program to scale

__Returns:__ A new program with the transformation matrix prepended and
coordinate parameters scaled.

__Note:__ The transformation matrix uses the reciprocal of the scale (1/scale)
to compensate for the scaled coordinates, maintaining the same visual output.
-}
optimizeScale :: Double -> Program -> Program
optimizeScale scale program
  | scale == 1.0 = program
  | scale == 0.0 = program
  | otherwise    = (   mkCommand GSSaveGS []
                    <| scaleMatrixCommand
                    <| fmap scaleCommand program
                   ) |> mkCommand GSRestoreGS []
  where
    invScale = 1.0 / scale

    -- Prepend the scaling transformation matrix: 1/scale 0 0 1/scale 0 0 cm
    scaleMatrixCommand :: Command
    scaleMatrixCommand = mkCommand GSSetCTM
      [ GFXNumber invScale
      , GFXNumber 0
      , GFXNumber 0
      , GFXNumber invScale
      , GFXNumber 0
      , GFXNumber 0
      ]

    -- Scale a single command's parameters
    scaleCommand :: Command -> Command
    scaleCommand cmd = case cOperator cmd of
      -- Path construction commands - scale all parameters
      GSMoveTo    -> scaleAllParams cmd
      GSLineTo    -> scaleAllParams cmd
      GSRectangle -> scaleAllParams cmd

      -- Curve commands - scale all parameters
      GSCubicBezierCurve    -> scaleAllParams cmd
      GSCubicBezierCurve1To -> scaleAllParams cmd
      GSCubicBezierCurve2To -> scaleAllParams cmd

      -- Text positioning commands - scale all parameters
      GSMoveToNextLine   -> scaleAllParams cmd
      GSMoveToNextLineLP -> scaleAllParams cmd
      GSSetTextMatrix    -> scaleCTMParams cmd

      -- Text commands - scale all parameters
      GSSetTextFont         -> scaleAllParams cmd
      GSSetWordSpacing      -> scaleAllParams cmd
      GSSetCharacterSpacing -> scaleAllParams cmd

      -- Line width - scale the width parameter
      GSSetLineWidth     -> scaleAllParams cmd

      -- CTM transformation - scale translation parameters (positions 4 and 5)
      GSSetCTM           -> scaleCTMParams cmd

      -- All other commands unchanged
      _anyOtherCommand   -> cmd

    -- Scale all numeric parameters
    scaleAllParams :: Command -> Command
    scaleAllParams cmd = cmd { cParameters = fmap scaleParam (cParameters cmd) }

    -- Scale a single parameter if it's a number
    scaleParam :: GFXObject -> GFXObject
    scaleParam (GFXNumber n) = GFXNumber (n * scale)
    scaleParam other         = other

    -- Scale only the translation parameters (e and f) of a CTM command
    scaleCTMParams :: Command -> Command
    scaleCTMParams cmd = case cParameters cmd of
      -- CTM format: a b c d e f
      -- Scale only e (index 4) and f (index 5) - the translation components
      a :<| b :<| c :<| d :<| e :<| f :<| rest ->
        cmd { cParameters =
                a <| b <| c <| d <| scaleParam e <| scaleParam f <| rest
            }
      _anyOtherMatrix -> cmd
