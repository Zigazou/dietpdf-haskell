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

import Data.ByteString (ByteString)
import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
  ( GFXObject (GFXName)
  , GSOperator (GSBeginMarkedContentSequencePL, GSMarkedContentPointPL, GSPaintShapeColourShading, GSPaintXObject, GSSetNonStrokeColorN, GSSetNonStrokeColorspace, GSSetParameters, GSSetStrokeColorN, GSSetStrokeColorspace, GSSetTextFont)
  )
import Data.PDF.Program (Program)
import Data.PDF.Resource
  ( Resource (ResColorSpace, ResExtGState, ResFont, ResPattern, ResProperties, ResShading, ResXObject)
  , resName
  )
import Data.Sequence (Seq (Empty, (:<|), (:|>)), (<|), (|>))
import Data.TranslationTable (TranslationTable, convert)

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
    program |> Command GSSetParameters
                       (rename ResExtGState resource <| mempty)

  go (Command GSPaintXObject (resource@GFXName{} :<| Empty)) program =
    program |> Command GSPaintXObject
                       (rename ResXObject resource <| mempty)

  go (Command GSSetStrokeColorN (components :|> resource@GFXName{})) program =
    program |> Command GSSetStrokeColorN
                       (components |> rename ResPattern resource)

  go ( Command GSSetNonStrokeColorN (components :|> resource@GFXName{})
     ) program =
    program |> Command GSSetNonStrokeColorN
                       (components |> rename ResPattern resource)

  go (Command GSSetTextFont (resource@GFXName{} :<| rest)) program =
    program |> Command GSSetTextFont
                       (rename ResFont resource <| rest)

  go ( Command GSPaintShapeColourShading (resource@GFXName{} :<| Empty)
     ) program =
    program |> Command GSPaintShapeColourShading
                       (rename ResShading resource <| mempty)

  go ( Command GSBeginMarkedContentSequencePL (tags :|> resource@GFXName{})
     ) program =
    program |> Command GSBeginMarkedContentSequencePL
                       (tags |> rename ResProperties resource)

  go ( Command GSBeginMarkedContentSequencePL (resource@GFXName{} :<| Empty)
     ) program =
    program |> Command GSBeginMarkedContentSequencePL
                       (rename ResProperties resource <| mempty)

  go (Command GSMarkedContentPointPL (tags :|> resource@GFXName{})) program  =
    program |> Command GSMarkedContentPointPL
                       (tags |> rename ResProperties resource)

  go (Command GSMarkedContentPointPL (resource@GFXName{} :<| Empty)) program  =
    program |> Command GSMarkedContentPointPL
                       (rename ResProperties resource <| mempty)

  go (Command GSSetNonStrokeColorspace (resource@GFXName{} :<| Empty)) program =
    program |> Command GSSetNonStrokeColorspace
                       (rename ResColorSpace resource <| mempty)

  go (Command GSSetStrokeColorspace (resource@GFXName{} :<| Empty)) program =
    program |> Command GSSetStrokeColorspace
                       (rename ResColorSpace resource <| mempty)

  go command program = program |> command

  -- Resource names are local to their category. The same name can identify
  -- both a color space and an ExtGState, including in different page scopes.
  rename :: (ByteString -> Resource) -> GFXObject -> GFXObject
  rename category (GFXName name) =
    GFXName (resName (convert table (category name)))

  rename _ object = object
