{-|
Extraction of referenced resources from PDF graphics programs

Identifies and collects all named resources referenced by a PDF graphics
program.

Scans a graphics program for all operator invocations that reference external
resources (fonts, images, graphics states, shadings, and marked content
properties) and returns them as a set. This is useful for determining which
resources must be included in the PDF document or which resources are actually
used by a page.

Resources tracked include:
* Graphics state parameters (ExtGState)
* Images and form XObjects
* Fonts
* Color patterns
* Shadings
* Marked content properties
-}
module PDF.Graphics.Interpreter.ResourcesUsed
  ( resourcesUsed
  ) where

import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
    ( GFXObject (GFXName)
    , GSOperator (GSBeginMarkedContentSequencePL, GSMarkedContentPointPL, GSPaintShapeColourShading, GSPaintXObject, GSSetNonStrokeColorN, GSSetParameters, GSSetStrokeColorN, GSSetTextFont)
    )
import Data.PDF.Program (Program)
import Data.PDF.Resource
    ( Resource (ResExtGState, ResFont, ResPattern, ResProperties, ResShading, ResXObject)
    )
import Data.Sequence (Seq (Empty, (:<|)))
import Data.Set (Set)
import Data.Set qualified as Set

{-|
Extract all resources referenced in a graphics program.

Scans a graphics program and collects all named resources that are referenced by
operators. Returns them as a set of typed resources (ExtGState, XObject,
Pattern, Font, Shading, Properties).

The analysis handles all operators that reference external resources:

* @gs@ operator with ExtGState parameter
* @Do@ operator with XObject (image or form) parameter
* @SCN/scn@ operators with Pattern parameter
* @Tf@ operator with Font parameter
* @sh@ operator with Shading parameter
* @BDC@ and @DP@ operators with marked content Properties parameter

Operators that don't reference named resources are skipped. The result is a set
of all unique resources discovered in the program.

@param program@ the graphics program to analyze @return@ a set of all resources
used in the program
-}
resourcesUsed :: Program -> Set Resource
resourcesUsed = foldl (flip go) mempty
 where
  {-
  Extract resources from a single command and add to the accumulator.

  Matches different operator types and extracts the referenced resource name,
  wrapping it in the appropriate resource type constructor (ExtGState, XObject,
  Pattern, Font, Shading, or Properties).

  For operators with multiple parameters, only the resource name parameter
  (typically the first or second parameter depending on the operator) is
  extracted. Other operators that don't reference resources return the identity
  function, leaving the accumulator unchanged.

  @param command@ the command to extract resources from @return@ a function that
  adds the extracted resource(s) to a set accumulator
  -}
  go :: Command -> Set Resource -> Set Resource
  go (Command GSSetParameters (GFXName resourceName :<| Empty)) =
    Set.insert (ResExtGState resourceName)
  go (Command GSPaintXObject (GFXName resourceName :<| Empty)) =
    Set.insert (ResXObject resourceName)
  go (Command GSSetStrokeColorN (GFXName resourceName :<| Empty)) =
    Set.insert (ResPattern resourceName)
  go (Command GSSetNonStrokeColorN (GFXName resourceName :<| Empty)) =
    Set.insert (ResPattern resourceName)
  go (Command GSSetTextFont (GFXName resourceName :<| _tail)) =
    Set.insert (ResFont resourceName)
  go (Command GSPaintShapeColourShading (GFXName resourceName :<| _tail)) =
    Set.insert (ResShading resourceName)
  go (Command GSBeginMarkedContentSequencePL (_tag :<| GFXName resourceName :<| Empty)) =
    Set.insert (ResProperties resourceName)
  go (Command GSMarkedContentPointPL (_tag :<| GFXName resourceName :<| Empty)) =
    Set.insert (ResProperties resourceName)
  go _anyOtherCommand = id
