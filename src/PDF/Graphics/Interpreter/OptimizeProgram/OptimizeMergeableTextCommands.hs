-- | Merge adjacent text displays without crossing state or marked-content barriers.
module PDF.Graphics.Interpreter.OptimizeProgram.OptimizeMergeableTextCommands
  ( optimizeMergeableTextCommands
  ) where

import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
  (GFXObject (GFXArray, GFXString), GSOperator (GSShowManyText, GSShowText))
import Data.PDF.Program (Program)
import Data.Sequence (Seq (Empty, (:<|)), (<|))
import Data.Sequence qualified as SQ
import PDF.Graphics.Interpreter.OptimizeParameters (convertHexString)

-- | Decode hex strings before concatenation, including odd-length hex strings.
textItems :: Command -> Maybe (Seq GFXObject)
textItems (Command GSShowText (value :<| Empty)) =
  case convertHexString value of
    string@GFXString{} -> Just (SQ.singleton string)
    _other -> Nothing
textItems (Command GSShowManyText (GFXArray values :<| Empty)) =
  Just (convertHexString <$> values)
textItems _other = Nothing

-- | Preserve positioning adjustments exactly; combine only adjacent strings.
compact :: Seq GFXObject -> Seq GFXObject
compact values@(GFXString{} :<| _) =
  let (strings, rest) = SQ.spanl isString values
      combined = BS.concat [bytes | GFXString bytes <- toList strings]
  in if BS.null combined
       then compact rest
       else GFXString combined <| compact rest
 where
  isString GFXString{} = True
  isString _other = False
compact (value :<| rest) = value <| compact rest
compact Empty = Empty

showItems :: Seq GFXObject -> Command
showItems (string@GFXString{} :<| Empty) = Command GSShowText (SQ.singleton string)
showItems values = Command GSShowManyText (SQ.singleton (GFXArray values))

-- | Consume a complete adjacent run in one traversal. Empty displays have no
-- positioning effect; quote operators are deliberately excluded.
optimizeMergeableTextCommands :: Program -> Program
optimizeMergeableTextCommands Empty = Empty
optimizeMergeableTextCommands (command :<| rest)
  | Just values <- textItems command = gather values rest
  | otherwise = command <| optimizeMergeableTextCommands rest
 where
  gather values (next :<| remaining)
    | Just more <- textItems next = gather (values <> more) remaining
  gather values remaining = case compact values of
    Empty -> optimizeMergeableTextCommands remaining
    items -> showItems items <| optimizeMergeableTextCommands remaining
