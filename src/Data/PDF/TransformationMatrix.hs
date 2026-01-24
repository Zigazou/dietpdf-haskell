{-|
2D affine transformation matrices and operations.

A transformation matrix in PDF shall be specified by six numbers, usually in the
form of an array containing six elements. In its most general form, this array
is denoted [ a b c d e f ].

Transformations:

- **Translations** shall be specified as [1 0 0 1 tx ty], where tx and ty shall
  be the distances to translate the origin of the coordinate system in the
  horizontal and vertical dimensions, respectively.
- **Scaling** shall be obtained by [sx 0 0 sy 0 0]. This scales the coordinates
  so that 1 unit in the horizontal and vertical dimensions of the new coordinate
  system is the same size as sx and sy units, respectively, in the previous
  coordinate system.
- **Rotations** shall be produced by [rc rs -rs rc 0 0], where rc = cos(q) and
  rs = sin(q) which has the effect of rotating the coordinate system axes by an
  angle q counter clockwise.
- **Skew** shall be specified by [1 wx wy 1 0 0], where wx = tan(a) and wy =
  tan(b) which skews the x axis by an angle a and the y axis by an angle b.
-}
module Data.PDF.TransformationMatrix
( TransformationMatrix (TransformationMatrix, tmA, tmB, tmC, tmD, tmE, tmF)
, transform
, matrixScale
, prod
) where

import Data.Kind (Type)

{-|
Categorization of simple transformations.

This is indicative and can be used for higher-level reasoning; all concrete
transformations are represented by 'TransformationMatrix'.
-}
type TransformationType :: Type
data TransformationType
  = Translation -- ^ Translation transformation
  | Scaling -- ^ Scaling transformation
  | Rotation -- ^ Rotation transformation
  | Skew -- ^ Skew transformation
  deriving stock (Eq, Show)

type TransformationMatrix :: Type
data TransformationMatrix = TransformationMatrix
  { tmA :: !Double -- ^ Element a (scale in X direction)
  , tmB :: !Double -- ^ Element b (shear in Y direction)
  , tmC :: !Double -- ^ Element c (shear in X direction)
  , tmD :: !Double -- ^ Element d (scale in Y direction)
  , tmE :: !Double -- ^ Element e (translation in X direction)
  , tmF :: !Double -- ^ Element f (translation in Y direction)
  } deriving stock (Eq, Show)

{-|
Transforms a point by the given transformation matrix.
-}
transform :: TransformationMatrix -> (Double, Double) -> (Double, Double)
transform (TransformationMatrix a b c d e f) (x, y) =
  (a * x + c * y + e, b * x + d * y + f)

{-|
Scales a point by the given transformation matrix.
-}
matrixScale :: TransformationMatrix -> (Double, Double) -> (Double, Double)
matrixScale (TransformationMatrix a b c d _e _f) (x, y) =
  (a * x + c * y, b * x + d * y)

{-|
Multiplies two transformation matrices.
-}
prod :: TransformationMatrix -> TransformationMatrix -> TransformationMatrix
prod (TransformationMatrix a1 b1 c1 d1 e1 f1)
     (TransformationMatrix a2 b2 c2 d2 e2 f2) =
  TransformationMatrix { tmA = a1 * a2 + c1 * b2
                       , tmB = b1 * a2 + d1 * b2
                       , tmC = a1 * c2 + c1 * d2
                       , tmD = b1 * c2 + d1 * d2
                       , tmE = a1 * e2 + c1 * f2 + e1
                       , tmF = b1 * e2 + d1 * f2 + f1
                       }

instance Semigroup TransformationMatrix where
  (<>) :: TransformationMatrix -> TransformationMatrix -> TransformationMatrix
  (<>) = prod

instance Monoid TransformationMatrix where
  mempty :: TransformationMatrix
  mempty = TransformationMatrix { tmA = 1
                                , tmB = 0
                                , tmC = 0
                                , tmD = 1
                                , tmE = 0
                                , tmF = 0
                                }