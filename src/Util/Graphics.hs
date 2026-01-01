{-|
Geometry helper for colinearity with tolerance.

This module provides a small geometry utility to test whether three points are
aligned and whether the middle point lies on the segment defined by the two
outer points. The check allows for a configurable tolerance to accommodate
floating-point rounding and imprecise inputs.
-}
module Util.Graphics (areAligned) where

{-|
Tests whether point @B@ lies on the line segment from @A@ to @C@ and that
vectors @AB@ and @BC@ are colinear within a given numeric tolerance.

Parameters:
- @A@: coordinates @(xa, ya)@
- @B@: coordinates @(xb, yb)@
- @C@: coordinates @(xc, yc)@
- @maxDifference@: maximum allowed absolute difference between the component
  ratios, computed as @abs ((vxAB / vxBC) - (vyAB / vyBC))@.

Returns 'True' when:
- @B@ lies inside the axis-aligned bounding box of @A@ and @C@; and
- either @B == C@ (degenerate trailing segment), or the absolute difference
  between the x- and y-component ratios is less than or equal to
  @maxDifference@.

Notes:
- If @A == B@, the function returns 'False' to ignore the trivial zero-length
  @AB@ case.
- Division by zero follows IEEE-754 semantics for 'Double'. Non-colinear cases
  will typically yield 'Infinity' or 'NaN' and thus return 'False'.
- Increase @maxDifference@ to make the colinearity test more tolerant to noisy
  or quantized coordinates.
-}
areAligned
  :: (Double, Double)
  -> (Double, Double)
  -> (Double, Double)
  -> Double
  -> Bool
areAligned (xa, ya) (xb, yb) (xc, yc) maxDifference
  | (xa, ya) == (xb, yb)           = False
  | not (xb >= minX && xb <= maxX) = False
  | not (yb >= minY && yb <= maxY) = False
  | vxBC == 0 && vyBC == 0         = True
  | otherwise                      = abs ((vxAB / vxBC) - (vyAB / vyBC))
                                     <= maxDifference
  where
    (minX, maxX) = (min xa xc, max xa xc)
    (minY, maxY) = (min ya yc, max ya yc)
    (vxAB, vyAB) = (xb - xa, yb - ya)
    (vxBC, vyBC) = (xc - xb, yc - yb)
