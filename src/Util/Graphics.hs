module Util.Graphics (areAligned) where

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
