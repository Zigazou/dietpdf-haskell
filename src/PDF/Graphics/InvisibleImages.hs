-- | Conservative visibility analysis. Only image invocations are removed;
-- resource dictionaries are left to the document's unused-resource pass.
module PDF.Graphics.InvisibleImages
  ( Rect (Rect), ImageInfo (ImageInfo), VisibilityMode (GeometryOnly, IncludeOcclusion)
  , Matrix, affine, bounds, balancedMarkedContent
  , removeInvisibleImages, removeInvisibleImagesWithMode ) where

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

-- | Coordinates in default page user space. Rational arithmetic avoids
-- rounding a near miss into a proof of containment.
type Rect :: Type
data Rect = Rect Rational Rational Rational Rational deriving stock (Eq, Show)

-- | Whether an image is known to paint its entire unit square opaquely.
type ImageInfo :: Type
newtype ImageInfo = ImageInfo Bool deriving stock (Eq, Show)

-- | Transparency groups permit geometric exclusion, but not our opaque-cover
-- proof.
type VisibilityMode :: Type
data VisibilityMode = GeometryOnly | IncludeOcclusion deriving stock (Eq, Show)

-- | Affine transformation matrix in the form:
-- (a, b, c, d, e, f) corresponds to the matrix:
-- [ a c e ]
-- [ b d f ]
-- [ 0 0 1 ]
type Matrix :: Type
type Matrix = (Rational, Rational, Rational, Rational, Rational, Rational)

-- | Graphics state, including the current transformation matrix, optional
-- clipping rectangle, and flags for fill and stroke operations.
type Graphics :: Type
data Graphics = Graphics Matrix (Maybe Rect) Bool Bool

-- | The path is deliberately outside the saved graphics state.
type Path :: Type
data Path = EmptyPath | Rectangle Rect | UnknownPath

affine :: Matrix -> Matrix -> Matrix
affine (a, b, c, d, e, f) (u, v, w, x, y, z) =
  ( a * u + c * v
  , b * u + d * v
  , a * w + c * x
  , b * w + d * x
  , a * y + c * z + e
  , b * y + d * z + f
  )

-- | Compute the axis-aligned bounding box of a rectangle after applying an
-- affine transformation.
bounds :: Matrix -> Rational -> Rational -> Rational -> Rational -> Rect
bounds (a, b, c, d, e, f) x y w h =
  let points =
        [
        ( a * i + c * j + e
        , b * i + d * j + f
        )
        | i <- [x, x + w], j <- [y, y + h]
        ]

  in
    Rect (minimum (map fst points))
         (minimum (map snd points))
         (maximum (map fst points))
         (maximum (map snd points))

axisAligned :: Matrix -> Bool
axisAligned (a, b, c, d, _, _) = (b == 0 && c == 0)
                              || (a == 0 && d == 0)

intersection :: Rect -> Rect -> Maybe Rect
intersection (Rect a b c d) (Rect e f g h)
  | max a e < min c g && max b f < min d h =
      Just (Rect (max a e) (max b f) (min c g) (min d h))
  | otherwise = Nothing

-- | Filled paths have antialiased boundaries. Keep those boundaries (including
-- zero-width regions) as potentially visible until another cover hides them.
-- Opaque images paint their closed raster footprint instead.
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
      else
        [original]
  | otherwise = case intersection original cover of
      Nothing -> [original]
      Just (Rect i j k l) -> filter nonempty [ Rect a b i d
                                             , Rect k b c d
                                             , Rect i b k j
                                             , Rect i l k d
                                             ]

 where nonempty (Rect x y z w) = x < z && y < w

-- | The viewport must be the effective MediaBox/CropBox intersection. Image
-- names must come from this page's resource scope. Unsupported syntax returns
-- the original program. Complex clipping remains an outer bound only, and
-- transparency, forms, shading and optional content cannot prove occlusion.
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
  | not (balancedMarkedContent 0 (toList program)) = program
  | otherwise =
      case
        walk
          0
          ( Graphics (1, 0, 0, 1, 0, 0)
                     (Just viewport)
                     True
                     (mode == IncludeOcclusion)
          )
          []
          EmptyPath
          False
          []
          IS.empty
          (toList program) of
        Nothing      -> program
        Just removed -> SQ.fromList
          [ command
          | (index, command) <- zip [0..] (toList program)
          , IS.notMember index removed
          ]
 where
  walk
    :: Int
    -> Graphics
    -> [Graphics]
    -> Path
    -> Bool
    -> [(Int, [Rect])]
    -> IntSet
    -> [Command]
    -> Maybe IntSet
  walk index
       state@(Graphics matrix clip exact opaque)
       stack
       path
       pending
       candidates
       removed
       commands =
    let
      next
        :: Graphics
        -> [Graphics]
        -> Path
        -> Bool
        -> [(Int, [Rect])]
        -> IntSet
        -> [Command]
        -> Maybe IntSet
      next = walk (index + 1)

      continue :: [Command] -> Maybe IntSet
      continue = next state stack path pending candidates removed

      barrier :: Graphics -> [Command] -> Maybe IntSet
      barrier newState = next newState stack path pending [] removed

      paint fill =
        let
          cover = case (fill, path, clip) of
            (True, Rectangle rectangle, Just clipping) | exact && opaque ->
              -- The page boundary cannot expose pixels outside the page. Keep
              -- the original fill edges when clipping is only the viewport.
              if clipping == viewport
                then Just rectangle
                else intersection rectangle clipping
            _anyOtherCase -> Nothing

          (remaining, deleted) = coverCandidates True cover candidates removed

          (clip', exact') = if pending
            then
              case path of
                Rectangle rectangle ->
                  (clip >>= (`intersection` rectangle), exact)
                _anyOtherPath ->
                  (clip, False)
            else
              (clip, exact)
        in next (Graphics matrix clip' exact' opaque)
                stack
                EmptyPath
                False
                remaining
                deleted

      imagePaint :: ByteString -> [Command] -> Maybe IntSet
      imagePaint name = case Map.lookup name images of
        Nothing -> barrier (Graphics matrix clip exact False)
        Just (ImageInfo solid) ->
          let
            visible = clip >>= intersection (bounds matrix 0 0 1 1)
            cover = if solid && opaque && exact && axisAligned matrix
                      then visible
                      else Nothing
            (remaining, deleted) = coverCandidates False
                                                   cover
                                                   candidates
                                                   removed
          in
            case visible of
              Nothing -> next state
                              stack
                              path
                              pending
                              remaining
                              (IS.insert index deleted)

              -- Bound candidate work on pages with many disjoint images.
              -- Forgotten candidates remain in the output.
              Just rectangle -> next state
                                     stack
                                     path
                                     pending
                                     (if mode == IncludeOcclusion
                                        then ( index
                                             , [rectangle]
                                             ):take 2047 remaining
                                        else []
                                     )
                                     deleted
    in case commands of
      [] | null stack && not pending -> Just removed
         | otherwise -> Nothing
      Command operator parameters : rest ->
        case (operator, toList parameters) of
          (GSBeginMarkedContentSequence, [GFXName tag]) | tag /= "OC" ->
            continue rest

          (GSBeginMarkedContentSequencePL, [GFXName tag, GFXDictionary _])
            | tag /= "OC" ->
            continue rest

          (GSBeginMarkedContentSequencePL, [GFXName tag, GFXName _])
            | tag /= "OC" ->
            continue rest

          (GSEndMarkedContentSequence, []) ->
            continue rest

          (GSSaveGS, []) ->
            next state (state:stack) path pending candidates removed rest

          (GSRestoreGS, []) -> case stack of
            saved:more -> next saved more path pending candidates removed rest
            []         -> Nothing

          (GSSetCTM, [ GFXNumber a
                     , GFXNumber b
                     , GFXNumber c
                     , GFXNumber d
                     , GFXNumber e
                     , GFXNumber f
                     ]) | all finite [a, b, c, d, e, f] ->
              next
                (Graphics (affine matrix ( toRational a
                                        , toRational b
                                        , toRational c
                                        , toRational d
                                        , toRational e
                                        , toRational f
                                        )
                          )
                          clip
                          exact
                          opaque
                )
                stack
                path
                pending
                candidates
                removed
                rest

          (GSPaintXObject, [GFXName name]) ->
            imagePaint name rest

          (GSRectangle, [GFXNumber x, GFXNumber y, GFXNumber w, GFXNumber h])
            | all finite [x, y, w, h] ->
              let newPath = case path of
                    EmptyPath | axisAligned matrix ->
                      Rectangle (bounds matrix
                                        (toRational x)
                                        (toRational y)
                                        (toRational w)
                                        (toRational h)
                                )
                    _anyOtherPath -> UnknownPath
              in next state stack newPath pending candidates removed rest

          (GSIntersectClippingPathNZWR, []) ->
            next state stack path True candidates removed rest

          (GSIntersectClippingPathEOR, []) ->
            next state stack path True candidates removed rest

          (op, []) | op `elem` fills ->
            paint True rest

          (GSEndPath, []) ->
            paint False rest

          (GSStrokePath, []) ->
            paint False rest

          (GSCloseStrokePath, []) ->
            paint False rest

          (op, _) | op `elem` [ GSMoveTo
                              , GSLineTo
                              , GSCubicBezierCurve
                              , GSCubicBezierCurve1To
                              , GSCubicBezierCurve2To
                              ] ->
            next state stack UnknownPath pending candidates removed rest

          (GSCloseSubpath, []) ->
            continue rest

          -- Text painting is a dependency barrier (Type 3 glyphs can execute
          -- arbitrary graphics). Text clipping requires a glyph interpreter.
          (GSBeginText, []) ->
            barrier state rest

          (GSSetTextRenderingMode, [GFXNumber renderingMode])
            | renderingMode `elem` [0, 1, 2, 3] ->
            continue rest

          (GSSetParameters, _) ->
            barrier (Graphics matrix clip exact False) rest

          (GSPaintShapeColourShading, _) ->
            barrier (Graphics matrix clip exact False) rest

          (op, _) | op `elem` [ GSSetNonStrokeColorspace
                              , GSSetNonStrokeColor
                              , GSSetNonStrokeColorN
                              ] ->
            next (Graphics matrix clip exact False)
                 stack
                 path
                 pending
                 candidates
                 removed
                 rest

          (op, _) | op `elem` harmless ->
            continue rest

          _anyOtherCase -> Nothing

  finite :: Double -> Bool
  finite value = not (isNaN value || isInfinite value)

  fills :: [GSOperator]
  fills =
    [ GSFillPathNZWR
    , GSFillPathEOR
    , GSFillStrokePathNZWR
    , GSFillStrokePathEOR
    , GSCloseFillStrokeNZWR
    , GSCloseFillStrokeEOR
    ]

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

  coverCandidates
    :: Bool
    -> Maybe Rect
    -> [(Int, [Rect])]
    -> IntSet
    -> ([(Int, [Rect])], IS.IntSet)
  coverCandidates _ Nothing candidates removed = (candidates, removed)
  coverCandidates filled (Just cover) candidates removed =
    foldr update ([], removed) candidates
   where
    update
      :: (Int, [Rect])
      -> ([(Int, [Rect])], IntSet)
      -> ([(Int, [Rect])], IntSet)
    update (index, regions) (remaining, deleted) =
      let
        visible = concatMap (\region -> subtractRect filled region cover)
                            regions
      in
        if null visible
          then
            (remaining, IS.insert index deleted)
          else
            -- Bound fragmentation; dropping a candidate keeps its invocation.
            if length visible > 256
              then (remaining, deleted)
              else ((index, visible):remaining, deleted)

{- | Marked-content nesting is independent of the q/Q graphics-state stack.
-}
balancedMarkedContent :: Int -> [Command] -> Bool
balancedMarkedContent depth [] = depth == 0
balancedMarkedContent depth (Command operator _ : rest)
  | operator == GSBeginMarkedContentSequence
    || operator == GSBeginMarkedContentSequencePL =
      balancedMarkedContent (depth + 1) rest
  | operator == GSEndMarkedContentSequence =
      depth > 0 && balancedMarkedContent (depth - 1) rest
  | otherwise = balancedMarkedContent depth rest
