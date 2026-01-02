{-|
Optimize PDF graphics programs by removing duplicate consecutive commands.

Provides utilities for eliminating redundant consecutive graphics state
operators, including color settings, line parameters, and rendering intents.
-}
module PDF.Graphics.Interpreter.OptimizeProgram.OptimizeDuplicates
  ( optimizeDuplicates
  ) where

import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
  ( GSOperator (GSSetColourRenderingIntent, GSSetFlatnessTolerance, GSSetLineCap, GSSetLineDashPattern, GSSetLineJoin, GSSetLineWidth, GSSetMiterLimit, GSSetNonStrokeCMYKColorspace, GSSetNonStrokeColor, GSSetNonStrokeColorN, GSSetNonStrokeColorspace, GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetParameters, GSSetStrokeCMYKColorspace, GSSetStrokeColor, GSSetStrokeColorN, GSSetStrokeColorspace, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace)
  )
import Data.PDF.Program (Program)
import Data.Sequence (Seq (Empty, (:<|)), (<|))

{-|
Test whether an operator can be safely removed when duplicated.

Operators that set state parameters can be removed if followed by an identical
operator setting the same state. Returns 'True' for color, line style, and
rendering intent operators; 'False' for others.
-}
uselessWhenDuplicated :: GSOperator -> Bool
uselessWhenDuplicated GSSetParameters              = True
uselessWhenDuplicated GSSetStrokeColor             = True
uselessWhenDuplicated GSSetNonStrokeColor          = True
uselessWhenDuplicated GSSetStrokeColorN            = True
uselessWhenDuplicated GSSetNonStrokeColorN         = True
uselessWhenDuplicated GSSetStrokeColorspace        = True
uselessWhenDuplicated GSSetNonStrokeColorspace     = True
uselessWhenDuplicated GSSetStrokeGrayColorspace    = True
uselessWhenDuplicated GSSetNonStrokeGrayColorspace = True
uselessWhenDuplicated GSSetStrokeRGBColorspace     = True
uselessWhenDuplicated GSSetNonStrokeRGBColorspace  = True
uselessWhenDuplicated GSSetStrokeCMYKColorspace    = True
uselessWhenDuplicated GSSetNonStrokeCMYKColorspace = True
uselessWhenDuplicated GSSetLineWidth               = True
uselessWhenDuplicated GSSetLineCap                 = True
uselessWhenDuplicated GSSetLineJoin                = True
uselessWhenDuplicated GSSetMiterLimit              = True
uselessWhenDuplicated GSSetLineDashPattern         = True
uselessWhenDuplicated GSSetColourRenderingIntent   = True
uselessWhenDuplicated GSSetFlatnessTolerance       = True
uselessWhenDuplicated _anyOtherOperator            = False

{-|
Test whether an operator sets stroke (outline) color.

Returns 'True' for stroke color-setting operators (SetStrokeColor,
SetStrokeColorN, SetStrokeRGBColorspace, SetStrokeCMYKColorspace,
SetStrokeGrayColorspace); 'False' for others.
-}
isStrokeColorOperator :: GSOperator -> Bool
isStrokeColorOperator GSSetStrokeColor          = True
isStrokeColorOperator GSSetStrokeColorN         = True
isStrokeColorOperator GSSetStrokeRGBColorspace  = True
isStrokeColorOperator GSSetStrokeCMYKColorspace = True
isStrokeColorOperator GSSetStrokeGrayColorspace = True
isStrokeColorOperator _anyOtherOperator         = False

{-|
Test whether an operator sets non-stroke (fill) color.

Returns 'True' for non-stroke color-setting operators (SetNonStrokeColor,
SetNonStrokeColorN, SetNonStrokeRGBColorspace, SetNonStrokeCMYKColorspace,
SetNonStrokeGrayColorspace); 'False' for others.
-}
isNonStrokeColorOperator :: GSOperator -> Bool
isNonStrokeColorOperator GSSetNonStrokeColor          = True
isNonStrokeColorOperator GSSetNonStrokeColorN         = True
isNonStrokeColorOperator GSSetNonStrokeRGBColorspace  = True
isNonStrokeColorOperator GSSetNonStrokeCMYKColorspace = True
isNonStrokeColorOperator GSSetNonStrokeGrayColorspace = True
isNonStrokeColorOperator _anyOtherOperator            = False

{-|
Remove all duplicate consecutive operators from a graphics program.

Iterates through the program and deletes redundant consecutive commands:

* Identical operators marked as useless when duplicated (color, line style,
  rendering intent) are removed if they appear consecutively
* Stroke color operators are removed if two consecutive stroke color operators
  appear (the first becomes redundant)
* Non-stroke color operators are removed if two consecutive non-stroke color
  operators appear (the first becomes redundant)

Returns a new program with all redundant consecutive commands removed.
-}
optimizeDuplicates :: Program -> Program
optimizeDuplicates Empty = mempty
optimizeDuplicates (command1@(Command operator1 _anyParameters1)
                :<| command2@(Command operator2 _anyParameters2)
                :<| rest)
  | operator1 == operator2 && uselessWhenDuplicated operator1
    = optimizeDuplicates (command2 <| rest)
  | isStrokeColorOperator operator1 && isStrokeColorOperator operator2
    = optimizeDuplicates (command2 <| rest)
  | isNonStrokeColorOperator operator1 && isNonStrokeColorOperator operator2
    = optimizeDuplicates (command2 <| rest)
  | otherwise = command1 <| optimizeDuplicates (command2 <| rest)
optimizeDuplicates (command :<| rest) = command <| optimizeDuplicates rest
