-- | Geometric bounds of bitmap XObject invocations in a graphics program.
module PDF.Graphics.ImageBounds
  ( imageBounds
  , boundingSize
  , xObjectMatrices
  , imageSize
  ) where

import Control.Monad (foldM)

import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
  ( GFXObject (GFXName)
  , GSOperator (GSPaintXObject, GSRestoreGS, GSSaveGS, GSSetCTM, GSUnknown)
  )
import Data.PDF.Program (Program)
import Data.Set (Set)
import Data.Set qualified as Set

import PDF.Graphics.Geometry
  (Matrix, Rect (Rect), affine, bounds, identity, matrixOf)
import PDF.Graphics.Visibility (numbers)

{- | Return one named bounding rectangle per bitmap @Do@ invocation, in
execution order, including repeated uses of the same image. The supplied names
must identify bitmap XObjects in the program's resource scope; other XObjects
are skipped. Form contents are not traversed.

Images occupy the unit square regardless of their pixel dimensions. The
interpreter applies the current transformation matrix to all four corners and
returns axis-aligned bounds in the program's initial user space. Clipping,
visibility and occlusion do not reduce these geometric bounds. The initial CTM
is the identity; callers analyzing a Form can prepend its placement matrix.

Malformed @cm@, @q@, @Q@ or @Do@ commands, non-finite matrix operands,
unbalanced saves/restores and unknown operators return 'Nothing', rather than
partially reliable results. Other operators do not change the CTM.
-}
imageBounds :: Set ByteString -> Program -> Maybe [(ByteString, Rect)]
imageBounds images program = do
  invocations <- xObjectMatrices identity program

  return [ (name, bounds matrix 0 0 1 1)
         | (name, matrix) <- invocations
         , Set.member name images
         ]

{- | Interpret all XObject placements with a caller-supplied initial CTM.
This includes Forms so document-level callers can recurse with their Matrix
and resource scope. Placements are returned in execution order.
-}
xObjectMatrices :: Matrix -> Program -> Maybe [(ByteString, Matrix)]
xObjectMatrices initial program = do
  (_, saved, rectangles) <- foldM step (initial, [], []) program

  if null saved
    then Just (reverse rectangles)
    else Nothing

 where
  step
    :: (Matrix, [Matrix], [(ByteString, Matrix)])
    -> Command
    -> Maybe (Matrix, [Matrix], [(ByteString, Matrix)])
  step state@(matrix, saved, rectangles) (Command operator parameters) =
    case (operator, toList parameters) of
      (GSSaveGS, []) -> Just (matrix, matrix : saved, rectangles)

      (GSRestoreGS, []) -> case saved of
        previous : rest -> Just (previous, rest, rectangles)
        []              -> Nothing

      (GSSetCTM, operands) -> do
        transformation <- numbers operands >>= matrixOf
        Just (affine matrix transformation, saved, rectangles)

      (GSPaintXObject, [GFXName name]) ->
        Just (matrix, saved, (name, matrix) : rectangles)

      (GSSaveGS, _) -> Nothing

      (GSRestoreGS, _) -> Nothing

      (GSPaintXObject, _) -> Nothing

      (GSUnknown _, _) -> Nothing

      _ -> Just state

{- | Lengths of the transformed image axes. Unlike axis-aligned rectangle
dimensions, these remain correct under rotation and reflection.
-}
imageSize :: Matrix -> (Double, Double)
imageSize (a, b, c, d, _, _) = (hypot a b, hypot c d)
 where
  hypot :: Rational -> Rational -> Double
  hypot x y = sqrt (fromRational (x * x + y * y))

-- | Width and height of an axis-aligned rectangle, in user-space units.
boundingSize :: Rect -> (Rational, Rational)
boundingSize (Rect left bottom right top) = ( abs (right - left)
                                            , abs (top - bottom)
                                            )
