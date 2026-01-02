{-|
Factorization of graphics state parameters into reusable resources

Optimizes PDF graphics programs by factorizing sequences of graphics state
setting commands into reusable external graphics state (ExtGState) resources.

Multiple consecutive graphics state commands (line width, line cap, line join,
miter limit, dash pattern, color rendering intent, text font, flatness) can be
replaced with a single @GSSetParameters@ command referencing a named ExtGState
resource. This reduces the size of graphics streams when the same state
combinations are used multiple times or can be shared across the PDF.

The module identifies contiguous sequences of factorizable commands and replaces
them with parameterized resource references.
-}
module PDF.Graphics.Interpreter.OptimizeGState
  ( optimizeGState
  )
where

import Data.Logging (Logging)
import Data.PDF.Command (Command (Command), mkCommand)
import Data.PDF.ExtGState (mkExtGState)
import Data.PDF.GFXObject
  ( GFXObject (GFXName)
  , GSOperator (GSSetColourRenderingIntent, GSSetFlatnessTolerance, GSSetLineCap, GSSetLineDashPattern, GSSetLineJoin, GSSetLineWidth, GSSetMiterLimit, GSSetParameters, GSSetTextFont)
  )
import Data.PDF.PDFObject (PDFObject (PDFDictionary))
import Data.PDF.PDFWork (PDFWork, addAdditionalGState)
import Data.PDF.Program (Program)
import Data.PDF.Resource (resName)
import Data.Sequence (Seq (Empty, (:<|)), spanl, (<|))

{-|
Test if a command can be factorized into an ExtGState resource.

A factorizable command is one that sets a graphics state parameter that can be
stored in an ExtGState dictionary. These include:

* Line width (w operator)
* Line cap style (J operator)
* Line join style (j operator)
* Miter limit (M operator)
* Line dash pattern (d operator)
* Color rendering intent (ri operator)
* Text font (Tf operator)
* Flatness tolerance (i operator)

Other commands are not factorizable and must be applied directly.
-}
isFactorizable :: Command -> Bool
isFactorizable (Command GSSetLineWidth _parameters)             = True
isFactorizable (Command GSSetLineCap _parameters)               = True
isFactorizable (Command GSSetLineJoin _parameters)              = True
isFactorizable (Command GSSetMiterLimit _parameters)            = True
isFactorizable (Command GSSetLineDashPattern _parameters)       = True
isFactorizable (Command GSSetColourRenderingIntent _parameters) = True
isFactorizable (Command GSSetTextFont _parameters)              = True
isFactorizable (Command GSSetFlatnessTolerance _parameters)     = True
isFactorizable _anyOtherCommand                                 = False

{-|
Test if a command cannot be factorized into an ExtGState resource.

Complementary predicate to @isFactorizable@. Returns @True@ for commands that
must be executed directly and cannot be stored in an ExtGState dictionary.
-}
isNotFactorizable :: Command -> Bool
isNotFactorizable = not . isFactorizable

{-|
Factorize graphics state parameters into reusable ExtGState resources.

Scans the program for contiguous sequences of factorizable graphics state
commands and replaces each sequence with a single @GSSetParameters@ command that
references a named ExtGState resource. Preserves non-factorizable commands in
their original positions.

The optimization works by:

* Finding the longest sequence of consecutive factorizable commands
* Creating an ExtGState dictionary from these commands
* Adding the dictionary as a resource to the PDF document
* Replacing the command sequence with a single @GSSetParameters@ call
  referencing the new resource by name
* Recursively processing the remaining program

Non-factorizable commands are collected and passed through without modification
until the next factorizable sequence is encountered.

@param program@ the graphics program to optimize @return@ the optimized program
with factorized graphics state settings
-}
optimizeGState :: Logging m => Program -> PDFWork m Program
optimizeGState Empty = return Empty
optimizeGState commands@(command :<| _rest) =
  if isFactorizable command
    then do
      let (factorizables, rest) = spanl isFactorizable commands

      resource <- addAdditionalGState (PDFDictionary $ mkExtGState factorizables)
      let factorized = mkCommand GSSetParameters
                                  [GFXName (resName resource)]

      optimizeds <- optimizeGState rest
      return $ factorized <| optimizeds
    else do
      -- No GState command to factorize, find the next one.
      let (notFactorizables, rest) = spanl isNotFactorizable commands
      optimizeds <- optimizeGState rest
      return $ notFactorizables <> optimizeds
