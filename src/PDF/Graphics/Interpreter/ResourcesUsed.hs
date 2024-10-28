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

{- |
The 'resourcesUsed' function takes a 'Program' and returns a list of
`PDFName`s giving the resources used in the program.

The resources used are the resources that are referenced by the program with
the `Do` and `gs` operators.
-}
resourcesUsed :: Program -> Set Resource
resourcesUsed = foldl (flip go) mempty
 where
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
