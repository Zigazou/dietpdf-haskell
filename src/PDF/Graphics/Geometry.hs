-- | Exact geometry for conservative visibility proofs.
module PDF.Graphics.Geometry (
  Rect (Rect),
  Matrix,
  identity,
  affine,
  bounds,
  transform,
  axisAligned,
  intersection,
  union,
  outside,
  matrixOf,
) where

import Data.Kind (Type)

{- | Coordinates in default page user space. Rational arithmetic avoids
rounding a near miss into a proof of containment.
-}
type Rect :: Type
data Rect = Rect Rational Rational Rational Rational deriving stock (Eq, Show)

{- | Affine transformation matrix in the form:
(a, b, c, d, e, f) corresponds to the matrix:
[ a c e ]
[ b d f ]
[ 0 0 1 ]
-}
type Matrix :: Type
type Matrix = (Rational, Rational, Rational, Rational, Rational, Rational)

{- | Compose two affine transformations using the usual column-vector order.
The resulting matrix is equivalent to applying the second transform and then the
first one.
-}
affine :: Matrix -> Matrix -> Matrix
affine (a, b, c, d, e, f) (u, v, w, x, y, z) =
  ( a * u + c * v
  , b * u + d * v
  , a * w + c * x
  , b * w + d * x
  , a * y + c * z + e
  , b * y + d * z + f
  )

{- | Compute the axis-aligned bounding box of a rectangle after applying an
affine transformation.
-}
bounds :: Matrix -> Rational -> Rational -> Rational -> Rational -> Rect
bounds (a, b, c, d, e, f) x y w h =
  let points =
        [ ( a * i + c * j + e
          , b * i + d * j + f
          )
        | i <- [x, x + w]
        , j <- [y, y + h]
        ]
   in Rect
        (minimum (map fst points))
        (minimum (map snd points))
        (maximum (map fst points))
        (maximum (map snd points))

{- | Determine whether an affine transformation preserves axis alignment.
This is used for conservative proofs that a transformed rectangle stays
axis-aligned and can therefore be treated as a simple rectangular cover.
-}
axisAligned :: Matrix -> Bool
axisAligned (a, b, c, d, _, _) =
  (b == 0 && c == 0)
    || (a == 0 && d == 0)

{- | Intersect two positive-area rectangles.
Touching edges alone are treated as non-overlapping because they do not create a
visible area of paint.
-}
intersection :: Rect -> Rect -> Maybe Rect
intersection (Rect a b c d) (Rect e f g h)
  | max a e < min c g && max b f < min d h =
      Just (Rect (max a e) (max b f) (min c g) (min d h))
  | otherwise = Nothing

{- | The identity affine transform.
It leaves points unchanged in default user space.
-}
identity :: Matrix
identity = (1, 0, 0, 1, 0, 0)

{- | Return the smallest axis-aligned rectangle that contains both operands.
-}
union :: Rect -> Rect -> Rect
union (Rect a b c d) (Rect e f g h) =
  Rect (min a e) (min b f) (max c g) (max d h)

{- | Check whether two rectangles are strictly disjoint.
Touching edges are still considered potentially visible because antialiasing can
paint pixels along the shared boundary.
-}
outside :: Rect -> Rect -> Bool
outside (Rect a b c d) (Rect e f g h) = c < e || g < a || d < f || h < b

{- | Apply an affine transformation to a rectangle by transforming its corners and
recomputing the enclosing axis-aligned bounds.
-}
transform :: Matrix -> Rect -> Rect
transform matrix (Rect a b c d) = bounds matrix a b (c - a) (d - b)

{- | Parse a list of six rational values as an affine transformation matrix.
The result is `Nothing` unless exactly six values are supplied.
-}
matrixOf :: [Rational] -> Maybe Matrix
matrixOf [a, b, c, d, e, f] = Just (a, b, c, d, e, f)
matrixOf _                  = Nothing
