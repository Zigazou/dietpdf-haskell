{-|
Small transformation helpers.

This module provides small helpers for applying transformations repeatedly.
-}
module Util.Transform
  ( untilNoChange
  , untilNoImprovement
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

{-|
Repeatedly transform a value, keeping the result with the smallest measure.
The input is the initial best result; ties retain the earlier best result.

Stop after the given number of consecutive attempts without a strict
improvement. Each new minimum resets this allowance. Transformations always
continue from the latest result, even when it is larger than the best result,
so temporary regressions and plateaus can lead to later improvements.
If a transformation returns the current value unchanged according to 'Eq',
stop immediately and return the best result encountered.

A non-positive allowance returns the input without applying the transformation.
There is no total iteration limit: a transformation that keeps finding new
minima can run indefinitely.
-}
untilNoImprovement
  :: (Eq a, Ord size)
  => Int
  -> (a -> size)
  -> (a -> a)
  -> a
  -> a
untilNoImprovement patience measure transform original
  | patience <= 0 = original
  | otherwise = go patience original (measure original) original
 where
  go remaining best bestSize current
    | remaining <= 0 = best
    | otherwise =
        let next = transform current
        in
          if next == current
            then best
            else
              let nextSize = measure next
              in
                if nextSize < bestSize
                  then go patience next nextSize next
                  else go (remaining - 1) best bestSize next
