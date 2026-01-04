{-|
Small transformation helpers.

This module provides small helpers for applying transformations repeatedly.
-}
module Util.Transform
  ( untilNoChange
  ) where

{-|
Apply a transformation repeatedly until reaching a fixed point or iteration
limit. The function applies @transform@ to the input value until the result
stops changing according to 'Eq'.

The final (stable) value is returned.
-}
untilNoChangeLimit :: Eq a => Int -> (a -> a) -> a -> a
untilNoChangeLimit 0 _transform original = original
untilNoChangeLimit limit transform original
  | original == transformed = transformed
  | otherwise = untilNoChangeLimit (limit - 1) transform transformed
 where
  transformed = transform original

{-|
Apply a transformation repeatedly until reaching a fixed point.

The function applies @transform@ to the input value until the result stops
changing according to 'Eq'. The final (stable) value is returned.
-}
untilNoChange :: Eq a => (a -> a) -> a -> a
untilNoChange = untilNoChangeLimit 64
