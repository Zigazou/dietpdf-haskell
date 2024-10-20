module PDF.Graphics.Interpreter.OptimizeProgram.OptimizeRectangle
  ( optimizeRectangle
  )
where

import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
    ( GFXObject (GFXNumber)
    , GSOperator (GSCloseSubpath, GSLineTo, GSMoveTo, GSRectangle)
    )
import Data.PDF.Program (Program)
import Data.Sequence (Seq (Empty, (:<|)), (<|))

{- |
Optimize line drawing commands to rectangle drawing commands.

This function replaces a sequence of line drawing commands that form a rectangle
like the following:
    x0,y0 -> x1,y1
      ^        |
      |        v
    x3,y3 <- x2,y2

or:
    x4,y4
    x0,y0 -> x1,y1
      ^        |
      |        v
    x3,y3 <- x2,y2
-}
optimizeRectangle :: Program -> Program
optimizeRectangle
    ( Command GSMoveTo p0@(GFXNumber x0 :<| GFXNumber y0 :<| Empty)
  :<| Command GSLineTo p1@(GFXNumber x1 :<| GFXNumber y1 :<| Empty)
  :<| Command GSLineTo p2@(GFXNumber x2 :<| GFXNumber y2 :<| Empty)
  :<| Command GSLineTo p3@(GFXNumber x3 :<| GFXNumber y3 :<| Empty)
  :<| Command GSCloseSubpath _closeSubpathParams
  :<| rest) =

  if x0 == x3 && y0 == y1 && x1 == x2 && y2 == y3
    then Command GSRectangle (GFXNumber x0 :<| GFXNumber y0
                          :<| GFXNumber (x2 - x0) :<| GFXNumber (y2 - y0)
                          :<| mempty)
      <| optimizeRectangle rest
    else Command GSMoveTo p0
      <| Command GSLineTo p1
      <| Command GSLineTo p2
      <| Command GSLineTo p3
      <| Command GSCloseSubpath mempty
      <| optimizeRectangle rest

optimizeRectangle
    ( Command GSMoveTo p0@(GFXNumber x0 :<| GFXNumber y0 :<| Empty)
  :<| Command GSLineTo p1@(GFXNumber x1 :<| GFXNumber y1 :<| Empty)
  :<| Command GSLineTo p2@(GFXNumber x2 :<| GFXNumber y2 :<| Empty)
  :<| Command GSLineTo p3@(GFXNumber x3 :<| GFXNumber y3 :<| Empty)
  :<| Command GSLineTo p4@(GFXNumber x4 :<| GFXNumber y4 :<| Empty)
  :<| Command GSCloseSubpath _closeSubpathParams
  :<| rest) =

  if x0 == x3 && y0 == y1 && x1 == x2 && y2 == y3 && x4 == x0 && y4 == y0
    then Command GSRectangle (GFXNumber x0 :<| GFXNumber y0
                          :<| GFXNumber (x2 - x0) :<| GFXNumber (y2 - y0)
                          :<| mempty)
      <| optimizeRectangle rest
    else Command GSMoveTo p0
      <| Command GSLineTo p1
      <| Command GSLineTo p2
      <| Command GSLineTo p3
      <| Command GSLineTo p4
      <| Command GSCloseSubpath mempty
      <| optimizeRectangle rest

optimizeRectangle (command :<| rest) = command <| optimizeRectangle rest

optimizeRectangle Empty = Empty
