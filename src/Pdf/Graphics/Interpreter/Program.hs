module Pdf.Graphics.Interpreter.Program
  ( Program
  , parseProgram
  , extractObjects
  , optimizeProgram
  )
where

import Control.Monad (foldM)
import Control.Monad.State (State, evalState)

import Data.Array (Array)
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Sequence (Seq ((:<|)), breakl, breakr, (<|), (|>))

import Pdf.Graphics.Interpreter.Command (Command (Command), optimizeCommand)
import Pdf.Graphics.Interpreter.GraphicsState
    ( GraphicsState
    , defaultGraphicsState
    )
import Pdf.Graphics.Object
    ( GFXObject (GFXOperator)
    , GSOperator (GSRestoreGS, GSSaveGS, GSUnknown)
    )
import Pdf.Graphics.Objects (Objects)

{- |
The 'Program' type represents a sequence of 'Command's.
-}
type Program :: Type
type Program = Array Command

onSave :: Command -> Bool
onSave (Command GSSaveGS _params) = True
onSave _anyOtherCommand           = False

onRestore :: Command -> Bool
onRestore (Command GSRestoreGS _params) = True
onRestore _anyOtherCommand              = False

{- |
Remove useless save/restore commands.
-}
removeUselessSaveRestore :: Program -> Program
removeUselessSaveRestore program = case breakl onRestore program of
  (beforeRestore, Command GSRestoreGS _params1 :<| Command GSRestoreGS _params2 :<| afterRestore) ->
    case breakr onSave beforeRestore of
      (beforeSave, Command GSSaveGS _params :<| afterSave) ->
           beforeSave
        <> afterSave
        <> (Command GSRestoreGS mempty <| removeUselessSaveRestore afterRestore)
      (beforeSave, afterSave) ->
           beforeSave
        <> afterSave
        <> (Command GSRestoreGS mempty <| removeUselessSaveRestore afterRestore)
  (beforeRestore, Command GSRestoreGS _params :<| afterRestore) ->
       beforeRestore
    <> (Command GSRestoreGS mempty <| removeUselessSaveRestore afterRestore)
  _anyOtherCase -> program

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
    collectCommands (objects, program) object =
      (objects |> object, program)

{- |
The 'optimizeProgram' function takes a 'Program' and returns an optimized
'Program'.
-}
optimizeProgram :: Program -> Program
optimizeProgram = flip evalState defaultGraphicsState
                . foldM go mempty
  --              . removeUselessSaveRestore
  where
    go :: Program -> Command -> State GraphicsState Program
    go program command = optimizeCommand command <&> (program |>)

{- |
The 'extractObjects' function takes a 'Program' and returns an array of
'GFXObject's.
-}
extractObjects :: Program -> Objects
extractObjects =
  foldl'
    (\acc (Command operator objects) -> acc <> objects |> GFXOperator operator)
    mempty
