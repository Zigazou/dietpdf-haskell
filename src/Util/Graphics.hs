module Util.Graphics (areAligned) where

areAligned
  :: (Double, Double)
  -> (Double, Double)
  -> (Double, Double)
  -> Double
  -> Bool
areAligned (xa, ya) (xb, yb) (xc, yc) maxArea =
  let
    -- Vectors AB and BC
    (dxAB, dyAB) = (xb - xa, yb - ya)
    (dxBC, dyBC) = (xc - xb, yc - yb)

    -- Angle between vectors AB and BC
    dotProduct = (dxAB * dxBC) + (dyAB * dyBC)
    magnitudeAB = sqrt (dxAB ** 2 + dyAB ** 2)
    magnitudeBC = sqrt (dxBC ** 2 + dyBC ** 2)
    cosTheta = dotProduct / (magnitudeAB * magnitudeBC)

    -- Convert the angle to degrees
    angle = acos (max (-1) (min 1 cosTheta)) * (180 / pi)

    -- Compute the area of the triangle
    area = abs ((xa * (yb - yc) + xb * (yc - ya) + xc * (ya - yb)) / 2.0)

  in (angle == 180) || ((angle >= 179 && angle < 180) && (area < maxArea))
