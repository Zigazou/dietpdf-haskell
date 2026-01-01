{-|
Representation of a content stream as commands.

This module provides a compact representation of a PDF content stream as a
sequence of 'Command' values.

A 'Program' is the result of grouping lower-level 'GFXObject' tokens into
operator invocations. Comments are ignored; inline images are handled as a
special operator that carries their image object.
-}
module Data.PDF.Program
  ( Program
  , mkProgram
  , parseProgram
  , extractObjects
  )
where

import Data.Array (Array)
import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
    ( GFXObject (GFXInlineImage, GFXOperator, GFXComment)
    , GSOperator (GSBeginInlineImage, GSUnknown)
    )
import Data.PDF.GFXObjects (GFXObjects)
import Data.Sequence ((|>))
import Data.Sequence qualified as SQ

{-|
The 'Program' type represents a sequence of 'Command's.
-}
type Program :: Type
type Program = Array Command

{-|
Construct a 'Program' from a list of commands.
-}
mkProgram :: [Command] -> Program
mkProgram = SQ.fromList

{-|
Parse a flat sequence of graphics objects into a command program.

This groups consecutive non-operator objects as arguments for the next operator
token.

If the input ends with pending objects (i.e. without a following operator), a
final 'GSUnknown' command is emitted to retain those objects.
-}
parseProgram :: GFXObjects -> Program
parseProgram objs =
  let (objects, program) = foldl' collectCommands (mempty, mempty) objs
  in  if null objects
    then program
    else program |> Command (GSUnknown "") objects
  where
    collectCommands
      :: (GFXObjects, Program)
      -> GFXObject
      -> (GFXObjects, Program)
    collectCommands (objects, program) (GFXOperator operator) =
      (mempty, program |> Command operator objects)
    collectCommands (objects, program) image@(GFXInlineImage _dict _data) =
      (mempty, program |> Command GSBeginInlineImage (objects |> image))
    collectCommands (objects, program) (GFXComment _comment) =
      (objects, program)
    collectCommands (objects, program) object =
      (objects |> object, program)

{-|
Flatten a 'Program' back into a sequence of 'GFXObject' tokens.

The operator of each command is re-emitted as a 'GFXOperator'. Inline image
commands contribute their embedded objects directly.
-}
extractObjects :: Program -> GFXObjects
extractObjects = foldl' go mempty
  where
    go :: GFXObjects -> Command -> GFXObjects
    go acc (Command GSBeginInlineImage objects) = acc <> objects
    go acc (Command operator objects) = acc <> objects |> GFXOperator operator
