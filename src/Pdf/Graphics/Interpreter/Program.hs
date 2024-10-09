module Pdf.Graphics.Interpreter.Program
  ( Program
  , mkProgram
  , parseProgram
  , extractObjects
  )
where

import Data.Array (Array)
import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.Sequence ((|>))
import Data.Sequence qualified as SQ

import Pdf.Graphics.Interpreter.Command (Command (Command))
import Pdf.Graphics.Object
    ( GFXObject (GFXInlineImage, GFXOperator)
    , GSOperator (GSBeginInlineImage, GSUnknown)
    )
import Pdf.Graphics.Objects (Objects)

{- |
The 'Program' type represents a sequence of 'Command's.
-}
type Program :: Type
type Program = Array Command

mkProgram :: [Command] -> Program
mkProgram = SQ.fromList

{- |
The 'parseProgram' function takes an array of 'GFXObject's and returns a
'Program'.
-}
parseProgram :: Objects -> Program
parseProgram objs =
  let (objects, program) = foldl' collectCommands (mempty, mempty) objs
  in  if null objects
    then program
    else program |> Command (GSUnknown "") objects
  where
    collectCommands :: (Objects, Program) -> GFXObject -> (Objects, Program)
    collectCommands (objects, program) (GFXOperator operator) =
      (mempty, program |> Command operator objects)
    collectCommands (objects, program) image@(GFXInlineImage _dict _data) =
      (mempty, program |> Command GSBeginInlineImage (objects |> image))
    collectCommands (objects, program) object =
      (objects |> object, program)

{- |
The 'extractObjects' function takes a 'Program' and returns an array of
'GFXObject's.
-}
extractObjects :: Program -> Objects
extractObjects = foldl' go mempty
  where
    go :: Objects -> Command -> Objects
    go acc (Command GSBeginInlineImage objects) = acc <> objects
    go acc (Command operator objects) = acc <> objects |> GFXOperator operator
