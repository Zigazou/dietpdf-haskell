{-|
Remove ineffective graphics commands from PDF streams.

Provides utilities for eliminating useless save/restore pairs and empty text
objects that do not affect the visual output.
-}
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


{-|
Test whether a command is a restore graphics state operation.

Returns 'True' for GSRestoreGS commands, 'False' otherwise.
-}
isRestore :: Command -> Bool
isRestore (Command GSRestoreGS _params) = True
isRestore _anyOtherCommand              = False

{-|
Test whether a command is a save graphics state operation.

Returns 'True' for GSSaveGS commands, 'False' otherwise.
-}
isSave :: Command -> Bool
isSave (Command GSSaveGS _params) = True
isSave _anyOtherCommand           = False

{-|
Test whether a command paints paths.

Returns 'True' for path painting operations (stroke, fill, close/stroke,
close/fill, paint XObject, paint shading, etc.), 'False' otherwise.
-}
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

{-|
Test whether a command shows text.

Returns 'True' for text showing operations (ShowText, NLShowText,
NLShowTextWithSpacing, ShowManyText), 'False' otherwise.
-}
isTextPaintingCommand :: Command -> Bool
isTextPaintingCommand (Command command _params) = case command of
  GSShowText              -> True
  GSNLShowText            -> True
  GSNLShowTextWithSpacing -> True
  GSShowManyText          -> True
  _anyOtherCommand        -> False

{-|
Test whether a command should be protected from removal.

Returns 'True' for commands that manage state or structure (begin/end
compatibility sections, inline images, marked content sequences, text objects,
and save/restore operations), 'False' for other commands. Protected commands are
never removed even if they appear ineffective.
-}
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

{-|
Test whether any painting command appears before a matching restore.

Scans through a program starting at the given nesting level, tracking
save/restore pairs. Returns 'True' if a painting command (path or text) is found
at the top level (level 0) before a restore operation. Returns 'False' if the
program ends or a restore is reached at level 0 without encountering any
painting commands. Used to determine if a save/restore pair is necessary.
-}
anyPaintingCommandBeforeRestore :: Int -> Program -> Bool
anyPaintingCommandBeforeRestore _anyLevel Empty = False
anyPaintingCommandBeforeRestore level (command :<| rest)
  | isSave command                  = anyPaintingCommandBeforeRestore (level + 1) rest
  | level > 0 && isRestore command  = anyPaintingCommandBeforeRestore (level - 1) rest
  | level == 0 && isRestore command = False
  | isPathPaintingCommand command   = True
  | isTextPaintingCommand command   = True
  | otherwise                       = anyPaintingCommandBeforeRestore level rest

{-|
Remove ineffective graphics commands from a PDF graphics program.

Eliminates commands that have no visual effect:

* __Empty text objects__: Consecutive BeginText/EndText pairs with no content
* __Useless save/restore pairs__: Save/restore pairs that have no painting
  operations between them (at the matching nesting level)

Preserves protected commands (state/structure management) and painting commands.
Recursively processes the entire program.
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
