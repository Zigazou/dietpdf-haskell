{- | Conservative visibility analysis. Only image invocations are removed;
resource dictionaries are left to the document's unused-resource pass.
-}
module PDF.Graphics.InvisibleImages (
  Rect (Rect),
  ImageInfo (ImageInfo),
  VisibilityMode (GeometryOnly, IncludeOcclusion),
  Matrix,
  affine,
  bounds,
  balancedMarkedContent,
  removeInvisibleImages,
  removeInvisibleImagesWithMode,
) where

import Control.Monad (foldM, guard)

import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
  ( GFXObject (GFXDictionary, GFXName, GFXNumber)
  , GSOperator (GSBeginCompatibilitySection, GSBeginMarkedContentSequence, GSBeginMarkedContentSequencePL, GSBeginText, GSCloseFillStrokeEOR, GSCloseFillStrokeNZWR, GSCloseStrokePath, GSCloseSubpath, GSCubicBezierCurve, GSCubicBezierCurve1To, GSCubicBezierCurve2To, GSEndCompatibilitySection, GSEndMarkedContentSequence, GSEndPath, GSEndText, GSFillPathEOR, GSFillPathNZWR, GSFillStrokePathEOR, GSFillStrokePathNZWR, GSIntersectClippingPathEOR, GSIntersectClippingPathNZWR, GSLineTo, GSMarkedContentPoint, GSMarkedContentPointPL, GSMoveTo, GSMoveToNextLine, GSMoveToNextLineLP, GSNLShowText, GSNLShowTextWithSpacing, GSNextLine, GSPaintShapeColourShading, GSPaintXObject, GSRectangle, GSRestoreGS, GSSaveGS, GSSetCTM, GSSetCharacterSpacing, GSSetColourRenderingIntent, GSSetFlatnessTolerance, GSSetHorizontalScaling, GSSetLineCap, GSSetLineDashPattern, GSSetLineJoin, GSSetLineWidth, GSSetMiterLimit, GSSetNonStrokeCMYKColorspace, GSSetNonStrokeColor, GSSetNonStrokeColorN, GSSetNonStrokeColorspace, GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetParameters, GSSetStrokeCMYKColorspace, GSSetStrokeColor, GSSetStrokeColorN, GSSetStrokeColorspace, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace, GSSetTextFont, GSSetTextLeading, GSSetTextMatrix, GSSetTextRenderingMode, GSSetTextRise, GSSetWordSpacing, GSShowManyText, GSShowText, GSStrokePath)
  )
import Data.PDF.Program (Program)
import Data.Sequence qualified as SQ

import PDF.Graphics.Geometry
  ( Matrix
  , Rect (Rect)
  , affine
  , axisAligned
  , bounds
  , identity
  , intersection
  , matrixOf
  )
import PDF.Graphics.Visibility (balancedMarkedContent, numbers)

-- | Whether an image is known to paint its entire unit square opaquely.
type ImageInfo :: Type
newtype ImageInfo = ImageInfo Bool deriving stock (Eq, Show)

{- | Transparency groups permit geometric exclusion, but not our opaque-cover
proof.
-}
type VisibilityMode :: Type
data VisibilityMode = GeometryOnly | IncludeOcclusion deriving stock (Eq, Show)

{- | Graphics state, including the current transformation matrix, optional
clipping rectangle, and flags for fill and stroke operations.
-}
type Graphics :: Type
data Graphics = Graphics Matrix (Maybe Rect) Bool Bool

-- | The path is deliberately outside the saved graphics state.
type Path :: Type
data Path = EmptyPath | Rectangle Rect | UnknownPath

{- | Split a rectangle into the visible remainder after subtracting a cover.
Filled paths preserve antialiased boundaries, while unfilled shapes only
subtract actual overlapping area.
-}
subtractRect :: Bool -> Rect -> Rect -> [Rect]
subtractRect filled original@(Rect a b c d) cover@(Rect e f g h)
  | not filled && e <= a && f <= b && g >= c && h >= d = []
  | filled =
      if a < g && e < c && b < h && f < d
        then
          [Rect a b e d | a <= e]
            ++ [Rect g b c d | g <= c]
            ++ [Rect (max a e) b (min c g) f | b <= f]
            ++ [Rect (max a e) h (min c g) d | h <= d]
        else [original]
  | otherwise = case intersection original cover of
      Nothing -> [original]
      Just (Rect i j k l) ->
        filter
          nonempty
          [ Rect a b i d
          , Rect k b c d
          , Rect i b k j
          , Rect i l k d
          ]
 where
  nonempty (Rect x y z w) = x < z && y < w

{- | The viewport must be the effective MediaBox/CropBox intersection. Image
names must come from this page's resource scope. Unsupported syntax returns the
original program. Complex clipping remains an outer bound only, and
transparency, forms, shading and optional content cannot prove occlusion.
-}
removeInvisibleImages
  :: Rect
  -> Map ByteString ImageInfo
  -> Program
  -> Program
removeInvisibleImages = removeInvisibleImagesWithMode IncludeOcclusion

{- | Select whether later painting may establish occlusion. Structural marked
content is preserved; optional content remains unsupported.
-}
removeInvisibleImagesWithMode
  :: VisibilityMode
  -> Rect
  -> Map ByteString ImageInfo
  -> Program
  -> Program
removeInvisibleImagesWithMode mode viewport images program
  | not (balancedMarkedContent 0 commands) = program
  | otherwise =
    case foldM (step mode viewport images) initial (zip [0 ..] commands) of
      Just final
        | null (saved final) && not (pendingClip final) ->
          SQ.fromList
            [ command
            | (index, command) <- zip [0 ..] commands
            , IS.notMember index (removed final)
            ]
      _ -> program
 where
  commands = toList program
  initial =
    Analysis
      (Graphics identity (Just viewport) True (mode == IncludeOcclusion))
      []
      EmptyPath
      False
      []
      IS.empty

{- | Analysis state for the visibility pass.

The path and candidate cover regions survive graphics-state saves and restores
so that occlusion can be checked across the full operation sequence.
-}
type Analysis :: Type
data Analysis = Analysis
  { graphics    :: Graphics
  , saved       :: [Graphics]
  , currentPath :: Path
  , pendingClip :: Bool
  , candidates  :: [(Int, [Rect])]
  , removed     :: IntSet
  }

-- | Interpret one command; any unsupported syntax rejects the entire analysis.
step
  :: VisibilityMode
  -> Rect
  -> Map ByteString ImageInfo
  -> Analysis
  -> (Int, Command)
  -> Maybe Analysis
step mode viewport images analysis (index, Command operator parameters) =
  case (operator, toList parameters, numbers (toList parameters)) of
    (GSBeginMarkedContentSequence, [GFXName tag], _) | tag /= "OC" ->
      keep

    (GSBeginMarkedContentSequencePL, [GFXName tag, GFXDictionary _], _)
      | tag /= "OC" -> keep

    (GSBeginMarkedContentSequencePL, [GFXName tag, GFXName _], _)
      | tag /= "OC" -> keep

    (GSEndMarkedContentSequence, [], _) ->
      keep

    (GSSaveGS, [], _) ->
      Just analysis{saved = graphics analysis : saved analysis}

    (GSRestoreGS, [], _) -> case saved analysis of
      state : rest -> Just analysis{graphics = state, saved = rest}
      []           -> Nothing

    (GSSetCTM, _, Just ns) -> do
      m <- matrixOf ns
      Just analysis{graphics = Graphics (affine matrix m) clip exact opaque}

    (GSPaintXObject, [GFXName name], _) ->
      Just (paintImage mode images index name analysis)

    (GSRectangle, _, Just [x, y, w, h]) ->
      let
        path = case currentPath analysis of
          EmptyPath | axisAligned matrix -> Rectangle (bounds matrix x y w h)
          _                              -> UnknownPath
       in
        Just analysis{currentPath = path}

    (GSIntersectClippingPathNZWR, [], _) ->
      Just analysis{pendingClip = True}

    (GSIntersectClippingPathEOR, [], _) ->
      Just analysis{pendingClip = True}

    (op, [], _) | op `elem` fills ->
      Just (paintPath viewport True analysis)

    (op, [], _) | op `elem` [GSEndPath, GSStrokePath, GSCloseStrokePath] ->
      Just (paintPath viewport False analysis)

    (op, _, _)
      | op
          `elem` [ GSMoveTo
                 , GSLineTo
                 , GSCubicBezierCurve
                 , GSCubicBezierCurve1To
                 , GSCubicBezierCurve2To
                 ] ->
          Just analysis{currentPath = UnknownPath}

    (GSCloseSubpath, [], _) -> keep

    -- Type 3 glyphs can execute arbitrary graphics; text is a dependency
    -- barrier.
    (GSBeginText, [], _) -> Just analysis{candidates = []}

    (GSSetTextRenderingMode, [GFXNumber renderingMode], _)
      | renderingMode `elem` [0, 1, 2, 3] -> keep

    (op, _, _)
      | op `elem` [GSSetParameters, GSPaintShapeColourShading] ->
      Just (disableOcclusion analysis){candidates = []}

    (op, _, _)
      | op
          `elem` [ GSSetNonStrokeColorspace
                 , GSSetNonStrokeColor
                 , GSSetNonStrokeColorN
                 ] ->
          Just (disableOcclusion analysis)

    (op, _, _) | op `elem` harmless -> keep

    _anyOtherCase -> Nothing
 where
  Graphics matrix clip exact opaque = graphics analysis
  keep = Just analysis

{- | Disable later occlusion proofs for the remaining graphics state.

This is used when the PDF content requests unsupported features or changes the
coloring context in a way that invalidates our cover reasoning.
-}
disableOcclusion :: Analysis -> Analysis
disableOcclusion analysis =
  let
    Graphics matrix clip exact _ = graphics analysis
  in
    analysis{graphics = Graphics matrix clip exact False}

{- | Apply the current path as a cover before committing the pending clipping
path.

A filled path contributes a conservative cover only when the fill is exact and
the current clipping region is known to be valid.
-}
paintPath :: Rect -> Bool -> Analysis -> Analysis
paintPath viewport fill analysis =
  analysis
    { graphics = Graphics matrix clip' exact' opaque
    , currentPath = EmptyPath
    , pendingClip = False
    , candidates = remaining
    , removed = deleted
    }
 where
  Graphics matrix clip exact opaque = graphics analysis

  cover :: Maybe Rect
  cover = do
    guard (fill && exact && opaque)
    Rectangle rectangle <- Just (currentPath analysis)
    clipping <- clip

    -- Preserve antialiased fill edges unless the only clipping is the viewport.
    if clipping == viewport
      then Just rectangle
      else intersection rectangle clipping

  (remaining, deleted) = coverCandidates True
                                         cover
                                         (candidates analysis)
                                         (removed analysis)

  (clip', exact')
    | not (pendingClip analysis) = (clip, exact)
    | otherwise = case currentPath analysis of
        Rectangle rectangle -> (clip >>= (`intersection` rectangle), exact)
        _anyOtherCase       -> (clip, False)

{- | Evaluate an image invocation against the current visibility state.

Opaque, axis-aligned images can contribute a cover; other image calls are
retained unless they are provably fully outside the viewport.
-}
paintImage
  :: VisibilityMode
  -> Map ByteString ImageInfo
  -> Int
  -> ByteString
  -> Analysis
  -> Analysis
paintImage mode images index name analysis = case Map.lookup name images of
  Nothing -> (disableOcclusion analysis){candidates = []}
  Just (ImageInfo solid) ->
    let
      visible :: Maybe Rect
      visible = clip >>= intersection (bounds matrix 0 0 1 1)

      cover :: Maybe Rect
      cover =
        if solid && opaque && exact && axisAligned matrix
          then visible
          else Nothing

      remaining :: [(Int, [Rect])]
      deleted :: IntSet
      (remaining, deleted) =
        coverCandidates False cover (candidates analysis) (removed analysis)
     in
      case visible of
        Nothing -> analysis { candidates = remaining
                            , removed = IS.insert index deleted
                            }

        Just rectangle ->
          analysis
            { candidates =
                if mode == IncludeOcclusion
                  then
                    -- Bound candidate work; forgotten candidates remain in
                    -- the output.
                    (index, [rectangle]) : take 2047 remaining
                  else
                    []
            , removed = deleted
            }
 where
  Graphics matrix clip exact opaque = graphics analysis

{- | Graphics operators that initiate filled path painting.

These are the operators that may contribute a geometric cover in the visibility
analysis.
-}
fills :: [GSOperator]
fills =
  [ GSFillPathNZWR
  , GSFillPathEOR
  , GSFillStrokePathNZWR
  , GSFillStrokePathEOR
  , GSCloseFillStrokeNZWR
  , GSCloseFillStrokeEOR
  ]

{- | Graphics operators that do not change the conservative visibility state.
-}
harmless :: [GSOperator]
harmless =
  [ GSSetLineWidth
  , GSSetLineCap
  , GSSetLineJoin
  , GSSetMiterLimit
  , GSSetLineDashPattern
  , GSSetColourRenderingIntent
  , GSSetFlatnessTolerance
  , GSSetStrokeColorspace
  , GSSetStrokeColor
  , GSSetStrokeColorN
  , GSSetStrokeGrayColorspace
  , GSSetStrokeRGBColorspace
  , GSSetStrokeCMYKColorspace
  , GSSetNonStrokeGrayColorspace
  , GSSetNonStrokeRGBColorspace
  , GSSetNonStrokeCMYKColorspace
  , GSEndText
  , GSMoveToNextLine
  , GSMoveToNextLineLP
  , GSSetTextMatrix
  , GSNextLine
  , GSShowText
  , GSNLShowText
  , GSNLShowTextWithSpacing
  , GSShowManyText
  , GSSetCharacterSpacing
  , GSSetWordSpacing
  , GSSetHorizontalScaling
  , GSSetTextLeading
  , GSSetTextFont
  , GSSetTextRise
  , GSMarkedContentPoint
  , GSMarkedContentPointPL
  , GSBeginCompatibilitySection
  , GSEndCompatibilitySection
  ]

{- | Remove candidates whose painted region is fully covered by the current
cover, keeping only the remaining visible fragments.
-}
coverCandidates
  :: Bool
  -> Maybe Rect
  -> [(Int, [Rect])]
  -> IntSet
  -> ([(Int, [Rect])], IS.IntSet)
coverCandidates _ Nothing pendingCandidates deletedIndices =
  (pendingCandidates, deletedIndices)
coverCandidates filled (Just cover) pendingCandidates deletedIndices =
  foldr update ([], deletedIndices) pendingCandidates
 where
  update
    :: (Int, [Rect])
    -> ([(Int, [Rect])], IntSet)
    -> ([(Int, [Rect])], IntSet)
  update (index, regions) (remaining, deleted) =
    let
      visible =
        concatMap
          (\region -> subtractRect filled region cover)
          regions
     in
      if null visible
        then (remaining, IS.insert index deleted)
        else
          -- Bound fragmentation; dropping a candidate keeps its invocation.
          if length visible > 256
            then (remaining, deleted)
            else ((index, visible) : remaining, deleted)
