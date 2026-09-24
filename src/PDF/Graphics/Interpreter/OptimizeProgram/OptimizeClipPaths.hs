{- | Remove provably redundant rectangular clips. Bounds describe a superset
of the effective clip; the initial page/device clip is deliberately unknown.
Complex paths and text clipping can only shrink these bounds. Invisible
unmarked paths, inline images and shadings are removed; text and marked
content are preserved. XObjects are retained because they may contain either.
-}
module PDF.Graphics.Interpreter.OptimizeProgram.OptimizeClipPaths (
  optimizeClipPaths,
) where

import Data.Foldable (foldl', toList)
import Data.IntSet qualified as IS
import Data.Kind (Type)
import Data.Maybe (isNothing)
import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
  ( GFXObject (GFXInlineImage, GFXNumber)
  , GSOperator (GSBeginCompatibilitySection, GSBeginInlineImage, GSBeginMarkedContentSequence, GSBeginMarkedContentSequencePL, GSBeginText, GSEndMarkedContentSequence, GSEndPath, GSEndText, GSFillPathEOR, GSFillPathNZWR, GSMarkedContentPoint, GSMarkedContentPointPL, GSPaintShapeColourShading, GSRectangle, GSRestoreGS, GSSaveGS, GSSetCTM, GSSetNonStrokeColorN, GSSetStrokeColorN, GSUnknown)
  )
import Data.PDF.OperatorCategory
  ( OperatorCategory (ClippingPathOperator, PathConstructionOperator, PathPaintingOperator)
  , category
  )
import Data.PDF.Program (Program)
import Data.Sequence qualified as SQ

import PDF.Graphics.Visibility (balancedMarkedContent)

-- | An affine transformation in PDF order: @a b c d e f@.
type Matrix :: Type
type Matrix = (Rational, Rational, Rational, Rational, Rational, Rational)

{- | An axis-aligned rectangle represented by its lower-left and upper-right
corners.
-}
type Rect :: Type
type Rect = (Rational, Rational, Rational, Rational)

-- | The currently known extent of the effective clipping region.
type Bounds :: Type
data Bounds
  = -- | No finite bounds are known.
    Unknown
  | -- | The clipping region is empty.
    EmptyClip
  | -- | The clipping region is bounded by a rectangle.
    Bounded Rect

-- | The path currently being constructed.
type Path :: Type
data Path
  = -- | No path is currently being constructed.
    NoPath
  | -- | A rectangle and the command index that created it.
    Rectangle Int Rect
  | -- | A path whose exact geometry is not tracked.
    OtherPath

-- | The identity affine transformation.
identity :: Matrix
identity = (1, 0, 0, 1, 0, 0)

-- | Extract finite numeric operands from a graphics-object sequence.
numbers :: SQ.Seq GFXObject -> Maybe [Rational]
numbers = traverse number . toList
 where
  -- \| Convert one finite graphics number to exact arithmetic.
  number (GFXNumber n) | not (isNaN n || isInfinite n) = Just (toRational n)
  number _ = Nothing

-- | Compose a current transformation matrix with six numeric operands.
transform :: Matrix -> [Rational] -> Maybe Matrix
transform (a, b, c, d, e, f) [g, h, i, j, k, l] =
  Just
    ( a * g + c * h
    , b * g + d * h
    , a * i + c * j
    , b * i + d * j
    , a * k + c * l + e
    , b * k + d * l + f
    )
transform _ _ = Nothing

-- | Transform a PDF rectangle when the matrix preserves axis alignment.
rectangle :: Matrix -> [Rational] -> Maybe Rect
rectangle (a, b, c, d, e, f) [x, y, w, h]
  | (b == 0 && c == 0) || (a == 0 && d == 0) =
      let
        x0 :: Rational
        x0 = a * x + c * y + e

        y0 :: Rational
        y0 = b * x + d * y + f

        x1 :: Rational
        x1 = a * (x + w) + c * (y + h) + e

        y1 :: Rational
        y1 = b * (x + w) + d * (y + h) + f
      in
        Just (min x0 x1, min y0 y1, max x0 x1, max y0 y1)
rectangle _ _ = Nothing

-- | Classify a rectangle, rejecting degenerate rectangles.
bounds :: Rect -> Bounds
bounds r@(x, y, u, v)
  | x >= u || y >= v
  = EmptyClip

  | otherwise
  = Bounded r

-- | Intersect known clipping bounds with a rectangle.
intersect :: Bounds -> Rect -> Bounds
intersect Unknown r = bounds r
intersect EmptyClip _ = EmptyClip
intersect (Bounded (x, y, u, v)) (x', y', u', v') =
  bounds (max x x', max y y', min u u', min v v')

-- | Test whether known bounds are contained in a rectangle.
contains :: Rect -> Bounds -> Bool
contains _ Unknown = False
contains _ EmptyClip = True
contains (x, y, u, v) (Bounded (x', y', u', v'))
  =  x <= x'
  && y <= y'
  && u >= u'
  && v >= v'

-- | A strict separation is required: touching edges may still be rasterized.
outside :: Rect -> Bounds -> Bool
outside _ Unknown = False
outside _ EmptyClip = True
outside (x, y, u, v) (Bounded (x', y', u', v'))
  =  u  < x'
  || u' < x
  || v  < y'
  || v' < y

-- | Test whether a clipping region is known to be empty.
isEmpty :: Bounds -> Bool
isEmpty EmptyClip = True
isEmpty _         = False

{- | Text and marked-content scopes are independent of q/Q. Protect complete
path lifetimes crossing a scope or marked-content point, including paths
constructed inside a scope but painted after EMC.
-}
protectedCommands :: [(Int, Command)] -> IS.IntSet
protectedCommands commands =
  third (foldl' step (0 :: Int, 0 :: Int, IS.empty) commands)
 where
  -- Extract the protected command indices from the traversal state.
  third :: (Int, Int, IS.IntSet) -> IS.IntSet
  third (_, _, result) = result

  -- Track scopes and mark commands that must not be optimized away.
  step :: (Int, Int, IS.IntSet) -> (Int, Command) -> (Int, Int, IS.IntSet)
  step (marked, text, protected) (index, Command op _) =
    let
      -- Update the marked-content nesting level based on the current command.
      marked' :: Int
      marked'
        | op `elem`
             [GSBeginMarkedContentSequence, GSBeginMarkedContentSequencePL]
        = marked + 1

        | op == GSEndMarkedContentSequence
        = max 0 (marked - 1)

        | otherwise
        = marked

      -- Update the text nesting level based on the current command.
      text' :: Int
      text'
          | op == GSBeginText = text + 1
          | op == GSEndText   = max 0 (text - 1)
          | otherwise         = text

      -- Determine whether the current command should be protected based on the
      -- marked-content and text nesting levels, as well as the command type.
      protect :: Bool
      protect = marked  > 0
             || marked' > 0
             || text    > 0
             || text'   > 0
             || op `elem` [GSMarkedContentPoint, GSMarkedContentPointPL]
    in
      ( marked'
      , text'
      , if protect
          then IS.insert index protected
          else protected
      )

-- | Command indices removed or terminated by the optimization.
type Edits :: Type
data Edits = Edits
  { deleted :: IS.IntSet
  , ended   :: IS.IntSet
  }

-- | Mark command indices for deletion.
remove :: [Int] -> Edits -> Edits
remove indices edits =
  edits{deleted = foldl' (flip IS.insert) (deleted edits) indices}

{- | Apply clips at path termination, after painting. Invisible painting with a
pending clip becomes n so the clipping side effect is preserved. Graphics state
changes are retained, including those needed after a later Q.
-}
optimizeClipPaths :: Program -> Program
optimizeClipPaths program
  | not (balancedMarkedContent 0 (toList program))
      || any unsupported (toList program)
  = program

  | otherwise
  = let
      -- Initialize the edits structure for tracking deletions and path
      -- terminations.
      edits :: Edits
      edits = walk (Just identity, Unknown)
                   []
                   NoPath
                   []
                   Nothing
                   (Edits IS.empty IS.empty)
                   commands
    in
      SQ.fromList
        [ if IS.member index (ended edits)
            then Command GSEndPath SQ.empty
            else command
        | (index, command) <- commands
        , IS.notMember index (deleted edits)
        ]
 where
  -- The set of command indices.
  commands :: [(Int, Command)]
  commands = zip [0 ..] (toList program)

  -- The set of command indices that are protected from deletion.
  protected :: IS.IntSet
  protected = protectedCommands commands

  -- Tiling patterns may contain text or marked content. Without their resource
  -- streams, preserve path painting whenever the program selects a pattern.
  -- Whether a color-setting command may select a tiling pattern.
  patternPainting :: Bool
  patternPainting = any selectsPattern (toList program)

  -- Detect color-setting commands that can select a pattern.
  selectsPattern :: Command -> Bool
  selectsPattern (Command op _) =
    op `elem` [GSSetNonStrokeColorN, GSSetStrokeColorN]

  -- Unknown extensions may change either geometry or semantic structure.
  -- Reject programs containing operators whose effects are not modeled.
  unsupported :: Command -> Bool
  unsupported (Command (GSUnknown _) _)               = True
  unsupported (Command GSBeginCompatibilitySection _) = True
  unsupported _                                       = False
  unprotected start end = case IS.lookupGE start protected of
    Just index -> index > end
    Nothing    -> True

  -- Walk the command stream while tracking graphics state and path edits.
  walk
    :: (Maybe Matrix, Bounds)
    -> [(Maybe Matrix, Bounds)]
    -> Path
    -> [Int]
    -> Maybe Int
    -> Edits
    -> [(Int, Command)]
    -> Edits
  walk _ _ _ _ _ edits [] = edits
  walk
    state@(matrix, clip)
    stack
    path
    pathIndices
    pending
    edits
    ((index, Command op params) : rest)
      | op == GSSaveGS
      = walk state (state : stack) path pathIndices pending edits rest

      | op == GSRestoreGS
      = case stack of
          saved : stack' ->
            walk saved stack' path pathIndices pending edits rest
          [] -> walk (Nothing, Unknown) [] path pathIndices pending edits rest

      | op == GSSetCTM
      = walk
          (matrix >>= \m -> numbers params >>= transform m, clip)
          stack
          path
          pathIndices
          pending
          edits
          rest

      | category op == PathConstructionOperator
      = let
          transformed :: Maybe Rect
          transformed = matrix >>= \m -> numbers params >>= rectangle m

          path' :: Path
          path' = case (path, op, transformed) of
                  (NoPath, GSRectangle, Just r) -> Rectangle index r
                  _                             -> OtherPath
        in
          walk state stack path' (index : pathIndices) pending edits rest

      | category op == ClippingPathOperator
      = walk
          state
          stack
          (case pending of
            Nothing -> path
            Just _ -> OtherPath
          )
          pathIndices
          (Just index)
          edits
          rest

      | category op == PathPaintingOperator
      = let
          safe :: Bool
          safe = unprotected (foldl' min index pathIndices) index

          invisible :: Bool
          invisible =
            not patternPainting
              && ( isEmpty clip || case path of
                    Rectangle _ r ->
                      op `elem` [GSFillPathNZWR, GSFillPathEOR]
                        && outside r clip
                    _ -> False
                  )

          clip' :: Bounds
          redundant :: Bool
          (clip', redundant) =
            case (pending, path) of
                (Just _, Rectangle _ r) -> (clip `intersect` r, contains r clip)
                _                       -> (clip, False)

          edits' :: Edits
          edits'
            | not safe
            = edits

            | invisible && (isNothing pending || redundant)
            = remove (index : pathIndices ++ maybe [] pure pending) edits

            | invisible
            = edits{ended = IS.insert index (ended edits)}

            | redundant
            = remove
                ( maybe [] pure pending
                    ++ if op == GSEndPath
                        then index : pathIndices
                        else []
                )
                edits

            | otherwise
            = edits
        in
          walk (matrix, clip') stack NoPath [] Nothing edits' rest

      | unprotected index index && invisibleObject
      = walk state stack path pathIndices pending (remove [index] edits) rest

      | otherwise
      = walk state stack path pathIndices pending edits rest
     where
      -- Whether the current command has no visible effect in the clip.
      invisibleObject
        | op == GSPaintShapeColourShading
        = isEmpty clip

        | op == GSBeginInlineImage
        = case toList params of
            [GFXInlineImage _ _] ->
              isEmpty clip
                || maybe
                  False
                  (`outside` clip)
                  (matrix >>= \m -> rectangle m [0, 0, 1, 1])
            _ -> False

        | otherwise
        = False
