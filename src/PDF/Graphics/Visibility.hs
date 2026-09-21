-- | Syntax validation shared by conservative visibility passes.
module PDF.Graphics.Visibility (balancedMarkedContent, numbers, finite) where

import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
  ( GFXObject (GFXNumber)
  , GSOperator (GSBeginMarkedContentSequence, GSBeginMarkedContentSequencePL, GSEndMarkedContentSequence)
  )

{- | Check whether a floating-point number is finite.

This is used to reject NaN and infinite values before converting PDF numbers to
`Rational` values.
-}
finite :: Double -> Bool
finite value = not (isNaN value || isInfinite value)

{- | Convert a list of graphics numeric objects to rational values.

The conversion fails if any element is not a finite numeric value.
-}
numbers :: [GFXObject] -> Maybe [Rational]
numbers = traverse $ \case
  GFXNumber n | finite n -> Just (toRational n)
  _anyOtherCase          -> Nothing

{- | Check whether marked-content begin/end nesting is balanced.

This ignores the separate graphics-state stack, so q/Q operators do not affect
validation.
-}
balancedMarkedContent :: Int -> [Command] -> Bool
balancedMarkedContent depth [] = depth == 0
balancedMarkedContent depth (Command operator _ : rest)
  | operator == GSBeginMarkedContentSequence
    || operator == GSBeginMarkedContentSequencePL =
    balancedMarkedContent (depth + 1) rest
  | operator == GSEndMarkedContentSequence =
    depth > 0 && balancedMarkedContent (depth - 1) rest
  | otherwise = balancedMarkedContent depth rest
