{-|
Small transformation helpers.

This module provides small helpers for applying transformations repeatedly.
-}
module Util.Transform
  ( untilNoChange
  ) where

{-|
Apply a transformation repeatedly until reaching a fixed point.

The function applies @transform@ to the input value until the result stops
changing according to 'Eq'. The final (stable) value is returned.

This will not terminate if @transform@ does not eventually reach a fixed point.
-}
untilNoChange :: Eq a => (a -> a) -> a -> a
untilNoChange transform original
  | original == transformed = transformed
  | otherwise               = untilNoChange transform transformed
  where transformed = transform original
