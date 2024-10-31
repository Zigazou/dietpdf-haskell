module PDF.Graphics.Interpreter.OptimizeProgram.OptimizeDuplicates
  ( optimizeDuplicates
  ) where

import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
    ( GSOperator (GSSetColourRenderingIntent, GSSetFlatnessTolerance, GSSetLineCap, GSSetLineDashPattern, GSSetLineJoin, GSSetLineWidth, GSSetMiterLimit, GSSetNonStrokeCMYKColorspace, GSSetNonStrokeColor, GSSetNonStrokeColorN, GSSetNonStrokeColorspace, GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetParameters, GSSetStrokeCMYKColorspace, GSSetStrokeColor, GSSetStrokeColorN, GSSetStrokeColorspace, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace)
    )
import Data.PDF.Program (Program)
import Data.Sequence (Seq (Empty, (:<|)), (<|))

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

{- |
Remove duplicated consecutive operators.
-}
optimizeDuplicates :: Program -> Program
optimizeDuplicates Empty = mempty
optimizeDuplicates (command1@(Command operator1 _anyParameters1)
                :<| command2@(Command operator2 _anyParameters2)
                :<| rest)
  | operator1 /= operator2 = command1 <| optimizeDuplicates (command2 <| rest)
  | uselessWhenDuplicated operator1 = optimizeDuplicates (command2 <| rest)
  | otherwise = command1 <| optimizeDuplicates (command2 <| rest)
optimizeDuplicates (command :<| rest) = command <| optimizeDuplicates rest
