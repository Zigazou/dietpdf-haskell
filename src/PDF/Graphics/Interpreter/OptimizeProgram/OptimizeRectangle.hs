{-|
Optimization of line paths to rectangle drawing commands

Optimizes PDF graphics programs by replacing sequences of line drawing
commands (MoveTo, LineTo, CloseSubpath) that form rectangles with the more
compact rectangle drawing operator (GSRectangle).

Detects both clockwise and counter-clockwise rectangular paths and converts
them to the GSRectangle command, which is both more compact and semantically
clearer in PDF graphics streams.
-}
module PDF.Graphics.Interpreter.OptimizeProgram.OptimizeRectangle
  ( optimizeRectangle
  )
where

import Data.Kind (Type)
import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
  ( GFXObject (GFXNumber)
  , GSOperator (GSCloseSubpath, GSLineTo, GSMoveTo, GSRectangle)
  )
import Data.PDF.Program (Program)
import Data.Sequence (Seq (Empty, (:<|)), (<|))

{-|
Represents the type of rectangle formed by a sequence of line coordinates.

This type indicates whether a set of point coordinates forms a valid rectangle
and, if so, whether the points traverse it in clockwise or counter-clockwise
order.
-}
type RectangleType :: Type
data RectangleType
  = ClockwiseRectangle
  | CounterClockwiseRectangle
  | NotARectangle

{-|
Classify a sequence of points as forming a rectangle.

Determines whether the given points (x0,y0), (x1,y1), (x2,y2), (x3,y3), (x4,y4)
form a valid rectangle. Returns:

* @ClockwiseRectangle@ if the points form a rectangle traversed clockwise
* @CounterClockwiseRectangle@ if the points form a rectangle traversed
  counter-clockwise
* @NotARectangle@ otherwise

The validation checks that the closing point (x4,y4) matches the starting point
(x0,y0), and that all sides are axis-aligned (either horizontal or vertical).
-}
isRectangle
  :: Double -> Double
  -> Double -> Double
  -> Double -> Double
  -> Double -> Double
  -> Double -> Double
  -> RectangleType
isRectangle x0 y0 x1 y1 x2 y2 x3 y3 x4 y4
  | x4 /= x0 || y4 /= y0                         = NotARectangle
  | x0 == x3 && y0 == y1 && x1 == x2 && y2 == y3 = ClockwiseRectangle
  | x0 == x1 && y1 == y2 && x2 == x3 && y3 == y0 = CounterClockwiseRectangle
  | otherwise                                    = NotARectangle

{-|
Optimize line drawing commands by converting rectangles to rectangle operators.

This function replaces a sequence of line drawing commands that form a rectangle
like the following:
    x0,y0 -> x1,y1        x0,y0 <- x3,y3
      ^        |     or     |        ^
      |        v            v        |
    x3,y3 <- x2,y2        x1,y1 -> x2,y2

or:
    x4,y4                 x4,y4
    x0,y0 -> x1,y1        x0,y0 <- x3,y3
      ^        |     or     |        ^
      |        v            v        |
    x3,y3 <- x2,y2        x1,y1 -> x2,y2

Detects sequences of MoveTo, LineTo, LineTo, LineTo, CloseSubpath commands (or
with an additional LineTo before closing) that form axis-aligned rectangles, and
replaces them with the more compact GSRectangle operator.

Handles both:

* Four-segment rectangles: MoveTo → LineTo → LineTo → LineTo → CloseSubpath
* Five-segment rectangles: MoveTo → LineTo → LineTo → LineTo → LineTo →
  CloseSubpath

The optimization recognizes both clockwise and counter-clockwise traversal
orders and converts them to the standard GSRectangle format with (x, y, width,
height) parameters. Non-rectangular paths are left unchanged. Recursively
processes the remaining program after each optimization.
-}
optimizeRectangle :: Program -> Program
optimizeRectangle
    (   Command GSMoveTo p0@(GFXNumber x0 :<| GFXNumber y0 :<| Empty)
    :<| Command GSLineTo p1@(GFXNumber x1 :<| GFXNumber y1 :<| Empty)
    :<| Command GSLineTo p2@(GFXNumber x2 :<| GFXNumber y2 :<| Empty)
    :<| Command GSLineTo p3@(GFXNumber x3 :<| GFXNumber y3 :<| Empty)
    :<| Command GSCloseSubpath _closeSubpathParams
    :<| rest
    ) =

  case isRectangle x0 y0 x1 y1 x2 y2 x3 y3 x0 y0 of
    ClockwiseRectangle ->
      Command GSRectangle (   GFXNumber x0        :<| GFXNumber y0
                          :<| GFXNumber (x2 - x0) :<| GFXNumber (y2 - y0)
                          :<| mempty
                          )
        <| optimizeRectangle rest

    CounterClockwiseRectangle ->
      Command GSRectangle (   GFXNumber x0        :<| GFXNumber y0
                          :<| GFXNumber (x2 - x0) :<| GFXNumber (y2 - y0)
                          :<| mempty
                          )
        <| optimizeRectangle rest

    NotARectangle -> Command GSMoveTo p0
                  <| Command GSLineTo p1
                  <| Command GSLineTo p2
                  <| Command GSLineTo p3
                  <| Command GSCloseSubpath mempty
                  <| optimizeRectangle rest

optimizeRectangle
    (   Command GSMoveTo p0@(GFXNumber x0 :<| GFXNumber y0 :<| Empty)
    :<| Command GSLineTo p1@(GFXNumber x1 :<| GFXNumber y1 :<| Empty)
    :<| Command GSLineTo p2@(GFXNumber x2 :<| GFXNumber y2 :<| Empty)
    :<| Command GSLineTo p3@(GFXNumber x3 :<| GFXNumber y3 :<| Empty)
    :<| Command GSLineTo p4@(GFXNumber x4 :<| GFXNumber y4 :<| Empty)
    :<| Command GSCloseSubpath _closeSubpathParams
    :<| rest
    ) =

  case isRectangle x0 y0 x1 y1 x2 y2 x3 y3 x4 y4 of
    ClockwiseRectangle ->
      Command GSRectangle (   GFXNumber x0        :<| GFXNumber y0
                          :<| GFXNumber (x2 - x0) :<| GFXNumber (y2 - y0)
                          :<| mempty
                          )
        <| optimizeRectangle rest

    CounterClockwiseRectangle ->
      Command GSRectangle (   GFXNumber x0        :<| GFXNumber y0
                          :<| GFXNumber (x2 - x0) :<| GFXNumber (y2 - y0)
                          :<| mempty
                          )
        <| optimizeRectangle rest

    NotARectangle -> Command GSMoveTo p0
                  <| Command GSLineTo p1
                  <| Command GSLineTo p2
                  <| Command GSLineTo p3
                  <| Command GSLineTo p4
                  <| Command GSCloseSubpath mempty
                  <| optimizeRectangle rest

optimizeRectangle (command :<| rest) = command <| optimizeRectangle rest

optimizeRectangle Empty = Empty
