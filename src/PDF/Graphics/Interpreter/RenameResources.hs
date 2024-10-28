module PDF.Graphics.Interpreter.RenameResources
  ( renameResources
  ) where

import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
  ( GFXObject (GFXName)
  , GSOperator (GSBeginMarkedContentSequencePL, GSMarkedContentPointPL, GSPaintShapeColourShading, GSPaintXObject, GSSetNonStrokeColorN, GSSetParameters, GSSetStrokeColorN, GSSetTextFont, GSSetNonStrokeColorspace, GSSetStrokeColorspace)
  )
import Data.PDF.Program (Program)
import Data.PDF.Resource
  ( Resource (ResColorSpace, ResExtGState, ResFont, ResPattern, ResProperties, ResShading, ResXObject)
  , resName
  )
import Data.Sequence (Seq (Empty, (:<|), (:|>)), (<|), (|>))
import Data.TranslationTable (TranslationTable, convert, hasTerm)

renameResources :: TranslationTable Resource -> Program -> Program
renameResources table = foldl (flip go) mempty
 where
  go :: Command -> Program -> Program
  go (Command GSSetParameters (resource@GFXName{} :<| Empty)) program =
    program
      |> Command GSSetParameters (rename resource <| mempty)

  go (Command GSPaintXObject (resource@GFXName{} :<| Empty)) program =
    program
      |> Command GSPaintXObject (rename resource <| mempty)

  go (Command GSSetStrokeColorN (resource@GFXName{} :<| Empty)) program =
    program
      |> Command GSSetStrokeColorN (rename resource <| mempty)

  go (Command GSSetNonStrokeColorN (resource@GFXName{} :<| Empty)) program =
    program
      |> Command GSSetNonStrokeColorN (rename resource <| mempty)

  go (Command GSSetTextFont (resource@GFXName{} :<| rest)) program =
    program
      |> Command GSSetTextFont (rename resource <| rest)

  go (Command GSPaintShapeColourShading (resource@GFXName{} :<| Empty)) program =
    program
      |> Command GSPaintShapeColourShading (rename resource <| mempty)

  go (Command GSBeginMarkedContentSequencePL (tags :|> resource@GFXName{})) program =
    program
      |> Command GSBeginMarkedContentSequencePL (tags |> rename resource)

  go (Command GSBeginMarkedContentSequencePL (resource@GFXName{} :<| Empty)) program =
    program
      |> Command GSBeginMarkedContentSequencePL (rename resource <| mempty)

  go (Command GSMarkedContentPointPL (tags :|> resource@GFXName{})) program  =
    program
      |> Command GSMarkedContentPointPL (tags |> rename resource)

  go (Command GSMarkedContentPointPL (resource@GFXName{} :<| Empty)) program  =
    program
      |> Command GSMarkedContentPointPL (rename resource <| mempty)

  go (Command GSSetNonStrokeColorspace (resource@GFXName{} :<| Empty)) program =
    program
      |> Command GSSetNonStrokeColorspace (rename resource <| mempty)

  go (Command GSSetStrokeColorspace (resource@GFXName{} :<| Empty)) program =
    program
      |> Command GSSetStrokeColorspace (rename resource <| mempty)

  go command program = program |> command

  rename :: GFXObject -> GFXObject
  rename (GFXName resourceName)
    | hasTerm table (ResExtGState resourceName)
      = GFXName (resName . convert table $ ResExtGState resourceName)
    | hasTerm table (ResColorSpace resourceName)
      = GFXName (resName . convert table $ ResColorSpace resourceName)
    | hasTerm table (ResFont resourceName)
      = GFXName (resName . convert table $ ResFont resourceName)
    | hasTerm table (ResShading resourceName)
      = GFXName (resName . convert table $ ResShading resourceName)
    | hasTerm table (ResProperties resourceName)
      = GFXName (resName . convert table $ ResProperties resourceName)
    | hasTerm table (ResPattern resourceName)
      = GFXName (resName . convert table $ ResPattern resourceName)
    | hasTerm table (ResXObject resourceName)
      = GFXName (resName . convert table $ ResXObject resourceName)
    | otherwise = GFXName resourceName
  rename anyOtherObject = anyOtherObject
