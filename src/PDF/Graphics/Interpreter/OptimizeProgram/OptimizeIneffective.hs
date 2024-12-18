module PDF.Graphics.Interpreter.OptimizeProgram.OptimizeIneffective
  ( optimizeIneffective
  , anyPaintingCommandBeforeRestore
  ) where

import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
  ( GSOperator (GSBeginCompatibilitySection, GSBeginInlineImage, GSBeginMarkedContentSequence, GSBeginMarkedContentSequencePL, GSBeginText, GSCloseFillStrokeEOR, GSCloseFillStrokeNZWR, GSCloseStrokePath, GSEndCompatibilitySection, GSEndInlineImage, GSEndMarkedContentSequence, GSEndPath, GSEndText, GSFillPathEOR, GSFillPathNZWR, GSFillStrokePathEOR, GSFillStrokePathNZWR, GSNLShowText, GSNLShowTextWithSpacing, GSPaintShapeColourShading, GSPaintXObject, GSRestoreGS, GSSaveGS, GSShowManyText, GSShowText, GSStrokePath)
  )
import Data.PDF.Program (Program)
import Data.Sequence (Seq (Empty, (:<|)), (<|))


isRestore :: Command -> Bool
isRestore (Command GSRestoreGS _params) = True
isRestore _anyOtherCommand              = False

isSave :: Command -> Bool
isSave (Command GSSaveGS _params) = True
isSave _anyOtherCommand           = False

isPathPaintingCommand :: Command -> Bool
isPathPaintingCommand (Command command _params) = case command of
  GSStrokePath              -> True
  GSCloseStrokePath         -> True
  GSFillPathNZWR            -> True
  GSFillPathEOR             -> True
  GSFillStrokePathNZWR      -> True
  GSFillStrokePathEOR       -> True
  GSCloseFillStrokeNZWR     -> True
  GSCloseFillStrokeEOR      -> True
  GSEndPath                 -> True
  GSPaintShapeColourShading -> True
  GSPaintXObject            -> True
  _anyOtherCommand          -> False

isTextPaintingCommand :: Command -> Bool
isTextPaintingCommand (Command command _params) = case command of
  GSShowText              -> True
  GSNLShowText            -> True
  GSNLShowTextWithSpacing -> True
  GSShowManyText          -> True
  _anyOtherCommand        -> False

protectedCommand :: Command -> Bool
protectedCommand (Command command _params) = case command of
  GSBeginCompatibilitySection    -> True
  GSEndCompatibilitySection      -> True
  GSBeginInlineImage             -> True
  GSEndInlineImage               -> True
  GSBeginMarkedContentSequence   -> True
  GSBeginMarkedContentSequencePL -> True
  GSEndMarkedContentSequence     -> True
  GSBeginText                    -> True
  GSEndText                      -> True
  GSSaveGS                       -> True
  GSRestoreGS                    -> True
  _anyOtherCommand               -> False

anyPaintingCommandBeforeRestore :: Int -> Program -> Bool
anyPaintingCommandBeforeRestore _anyLevel Empty = False
anyPaintingCommandBeforeRestore level (command :<| rest)
  | isSave command                  = anyPaintingCommandBeforeRestore (level + 1) rest
  | level > 0 && isRestore command  = anyPaintingCommandBeforeRestore (level - 1) rest
  | level == 0 && isRestore command = False
  | isPathPaintingCommand command   = True
  | isTextPaintingCommand command   = True
  | otherwise                       = anyPaintingCommandBeforeRestore level rest

{- |
Remove useless save/restore graphics state commands or begin/end text.
-}
optimizeIneffective :: Program -> Program
optimizeIneffective Empty = mempty
optimizeIneffective (   Command GSBeginText _noParameters1
                    :<| Command GSEndText _noParameters2
                    :<| rest
                    ) = optimizeIneffective rest
optimizeIneffective (command :<| rest)
  | isPathPaintingCommand command          = command <| optimizeIneffective rest
  | isTextPaintingCommand command          = command <| optimizeIneffective rest
  | protectedCommand command               = command <| optimizeIneffective rest
  | anyPaintingCommandBeforeRestore 0 rest = command <| optimizeIneffective rest
  | otherwise                              = optimizeIneffective rest
