{-|
Resource name translation in PDF graphics commands

Applies a translation table to rename resources referenced in PDF graphics
commands. This allows resources to be consistently renamed throughout a graphics
program (e.g., shortening resource names during optimization).

Handles all operator types that reference named resources:

* Graphics state parameters (gs, ri)
* Image/form XObject rendering (Do)
* Color specifications (SCN, scn)
* Font selection (Tf)
* Shadings (sh)
* Marked content sequences (BMC, BDC, MP, DP)
* Color space settings (CS, cs)

The translation table maps original resource names to new names across all
resource categories (fonts, images, graphics states, etc.). Resources not
present in the translation table are left unchanged.
-}
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

{-|
Apply resource name translations to a graphics program.

Processes each command in the program and renames all resource references
according to the provided translation table. Handles all operators that take
resource name parameters.

The translation is applied in order through the program, accumulating the
results. Commands with resource references are updated with translated names;
other commands pass through unchanged.

@param table@ the translation table mapping original names to new names @param
program@ the graphics program whose resources to rename @return@ the program
with all resource names translated
-}
renameResources :: TranslationTable Resource -> Program -> Program
renameResources table = foldl (flip go) mempty
 where
  {-
  Process a single command and accumulate it to the output program.

  Examines the command type and parameter structure. If the command references a
  resource name, translates it using the helper function. Otherwise, passes the
  command through unchanged.

  Handles all operator types that take resource parameters, including operators
  with single parameters and operators with multiple parameters where only the
  resource name is renamed.

  @param command@ the command to process @param program@ the accumulated output
  program @return@ the updated program with this command appended (with
  translations applied)
  -}
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

  {-
  Translate a resource name according to the translation table.

  Checks if the given object is a name and, if so, queries the translation table
  to find if a translation exists for any resource type (ExtGState, ColorSpace,
  Font, Shading, Properties, Pattern, or XObject).

  The translation is checked in a specific order, returning the first match
  found. If no translation exists, the original name is returned unchanged.

  Names that don't match any resource type or are not name objects are returned
  as-is.

  @param object@ the GFX object (typically a name) to translate @return@ the
  translated object with the new resource name, or the original if not
  translated
  -}
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
