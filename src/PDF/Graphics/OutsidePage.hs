{- | Remove painting whose conservative bounds lie strictly outside the page.
Bounds are in default user space; clipping paths and text positioning remain
effective even when their associated painting is discarded.
-}
module PDF.Graphics.OutsidePage (FontInfo (FontInfo), removeOutsidePage) where

import Control.Monad (foldM)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (foldl', toList)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
  ( GFXObject (GFXArray, GFXHexString, GFXInlineImage, GFXName, GFXNumber, GFXString)
  , GSOperator (GSBeginCompatibilitySection, GSBeginInlineImage, GSBeginMarkedContentSequence, GSBeginMarkedContentSequencePL, GSBeginText, GSCloseFillStrokeEOR, GSCloseFillStrokeNZWR, GSCloseStrokePath, GSCloseSubpath, GSCubicBezierCurve, GSCubicBezierCurve1To, GSCubicBezierCurve2To, GSEndCompatibilitySection, GSEndMarkedContentSequence, GSEndPath, GSEndText, GSFillPathEOR, GSFillPathNZWR, GSFillStrokePathEOR, GSFillStrokePathNZWR, GSIntersectClippingPathEOR, GSIntersectClippingPathNZWR, GSLineTo, GSMarkedContentPoint, GSMarkedContentPointPL, GSMoveTo, GSMoveToNextLine, GSMoveToNextLineLP, GSNLShowText, GSNLShowTextWithSpacing, GSNextLine, GSPaintShapeColourShading, GSPaintXObject, GSRectangle, GSRestoreGS, GSSaveGS, GSSetCTM, GSSetCharacterSpacing, GSSetColourRenderingIntent, GSSetFlatnessTolerance, GSSetHorizontalScaling, GSSetLineCap, GSSetLineDashPattern, GSSetLineJoin, GSSetLineWidth, GSSetMiterLimit, GSSetNonStrokeCMYKColorspace, GSSetNonStrokeColor, GSSetNonStrokeColorN, GSSetNonStrokeColorspace, GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetParameters, GSSetStrokeCMYKColorspace, GSSetStrokeColor, GSSetStrokeColorN, GSSetStrokeColorspace, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace, GSSetTextFont, GSSetTextLeading, GSSetTextMatrix, GSSetTextRenderingMode, GSSetTextRise, GSSetWordSpacing, GSShowManyText, GSShowText, GSStrokePath)
  )
import Data.PDF.Program (Program)
import Data.Sequence qualified as SQ
import Data.Word (Word8)

import PDF.Graphics.Geometry
  ( Matrix
  , Rect (Rect)
  , affine
  , bounds
  , identity
  , matrixOf
  , outside
  , transform
  , union
  )
import PDF.Graphics.Visibility (balancedMarkedContent, finite, numbers)

import Util.Hex (fromHexDigits)

{- | Simple horizontal fonts with reliable descriptor bounds and explicit
widths. Type 3 and composite fonts are deliberately excluded by the resource
reader.
-}
type FontInfo :: Type
data FontInfo = FontInfo Rect (Map Int Rational)

type State :: Type
data State = State
  { ctm         :: Matrix
  , stroke      :: Maybe (Rational, Rational)
  , font        :: Maybe FontInfo
  , size        :: Rational
  , spacing     :: Rational
  , wordSpacing :: Rational
  , horizontal  :: Rational
  , leading     :: Rational
  , rise        :: Rational
  , rendering   :: Rational
  }

type Path :: Type
data Path = Path (Maybe Rect) [Int] Bool

type TextPosition :: Type
data TextPosition = TextPosition (Maybe Matrix) (Maybe Matrix) [Int]
  deriving stock (Eq)

{- | Compute the conservative bounding box of a stroked path segment.

The width is expanded by the transform-dependent row norm so that square caps,
miter joins, and wide strokes remain safely enclosed.
-}
strokeBounds :: State -> Rect -> Maybe Rect
strokeBounds state (Rect a b c d) = do
  (width, miter) <- stroke state
  if width <= 0
    then
      Nothing
    else do
      let
        (u, v, w, x, _, _) = ctm state
        radius = width * max 2 miter
        dx = radius * (abs u + abs w)
        dy = radius * (abs v + abs x)

      return (Rect (a - dx) (b - dy) (c + dx) (d + dy))

{- | Text candidates are deleted only at a positioning reset. An off-page show
before a visible show must remain, since it advances the text matrix. This
avoids introducing rounded replacement advances into the stream.
-}
removeOutsidePage
  :: Rect
  -> Map ByteString FontInfo
  -> Map ByteString Rect
  -> Program
  -> Program
removeOutsidePage viewport fonts objects program
  | not (balancedMarkedContent 0 commands) = program
  | otherwise =
    case foldM (step viewport fonts objects) initial (zip [0 ..] commands) of
      Just final | complete final ->
        SQ.fromList
          [ if IS.member index (ended final)
              then Command GSEndPath mempty
              else command
          | (index, command) <- zip [0 ..] commands
          , IS.notMember index (deleted final)
          ]
      _anyOtherCase -> program
 where
  commands :: [Command]
  commands = toList program

  initial :: Analysis
  initial =
    Analysis
      (State identity (Just (1, 10)) Nothing 0 0 0 1 0 0 0)
      []
      (Path Nothing [] False)
      Nothing
      IS.empty
      IS.empty

-- | Painting candidates are separate from the PDF graphics-state stack.
type Analysis :: Type
data Analysis = Analysis
  { graphics    :: State
  , saved       :: [State]
  , currentPath :: Path
  , text        :: Maybe TextPosition
  , deleted     :: IntSet
  , ended       :: IntSet
  }

{- | Determine whether the analysis has reached a stable, self-contained state.
A complete pass means no pending graphics-state stack entries, no active text
position tracking, and no open clipping geometry waiting to be settled.
-}
complete :: Analysis -> Bool
complete analysis = null (saved analysis)
                 && isNothing (text analysis)
                 && not clipping
 where
  Path _ _ clipping = currentPath analysis

{- | Interpret one graphics operator and update the conservative off-page check.
Unsupported syntax is rejected so the caller can leave the PDF stream unchanged.
-}
step
  :: Rect
  -> Map ByteString FontInfo
  -> Map ByteString Rect
  -> Analysis
  -> (Int, Command)
  -> Maybe Analysis
step viewport fonts objects analysis (index, Command op args) =
  case (op, toList args, numbers (toList args)) of
    (GSSaveGS, [], _) ->
      Just analysis{saved = state : saved analysis}

    (GSRestoreGS, [], _) -> case saved analysis of
      previous : rest -> Just analysis{graphics = previous, saved = rest}
      []              -> Nothing

    (GSSetCTM, _, Just ns) -> do
      m <- matrixOf ns
      update state{ctm = affine (ctm state) m}

    (GSSetLineWidth, _, Just [w])
      | w >= 0 ->
          update state{stroke = fmap (\(_, m) -> (w, m)) (stroke state)}

    (GSSetMiterLimit, _, Just [m])
      | m >= 1 ->
          update state{stroke = fmap (\(w, _) -> (w, m)) (stroke state)}

    -- ExtGState can change stroke parameters and the selected font.
    (GSSetParameters, _, _) -> update state{stroke = Nothing, font = Nothing}

    (GSRectangle, _, Just [x, y, w, h]) ->
      add [x, y, x + w, y, x, y + h, x + w, y + h]

    (GSMoveTo, _, Just [x, y]) ->
      add [x, y]

    (GSLineTo, _, Just [x, y]) | not (null indices) ->
      add [x, y]

    (GSCubicBezierCurve, _, Just ns) | length ns == 6 && not (null indices) ->
      add ns

    (curve, _, Just ns)
      | curve `elem` [GSCubicBezierCurve1To, GSCubicBezierCurve2To]
        && length ns == 4
        && not (null indices) ->
      add ns

    (GSCloseSubpath, [], _)
      | not (null indices) ->
      Just analysis{currentPath = Path box (index : indices) clipping}

    (clip, [], _)
      | clip `elem` [GSIntersectClippingPathNZWR, GSIntersectClippingPathEOR] ->
      Just analysis{currentPath = Path box indices True}

    (_, [], _) | op `elem` pathEndings ->
      Just (paintPath viewport index op analysis)

    (GSPaintXObject, [GFXName name], _) ->
      discard
        ( maybe False
                (outside viewport . transform (ctm state))
                (Map.lookup name objects)
        )

    (GSBeginInlineImage, [GFXInlineImage _ _], _) ->
      discard (outside viewport (bounds (ctm state) 0 0 1 1))

    _anyOtherCase -> stepText viewport fonts index op (toList args) analysis
 where
  state = graphics analysis
  Path box indices clipping = currentPath analysis
  update newState = Just analysis{graphics = newState}
  add coords = Just (addPoints index coords analysis)
  discard off =
    Just
      analysis
        { deleted =
            if off
              then IS.insert index (deleted analysis)
              else deleted analysis
        }

{- | Handle text state changes and positioning operations in the off-page pass.
These operations advance the text matrix and therefore must be tracked with
separate dependency rules from ordinary path painting.
-}
stepText
  :: Rect
  -> Map ByteString FontInfo
  -> Int
  -> GSOperator
  -> [GFXObject]
  -> Analysis
  -> Maybe Analysis
stepText viewport fonts index op args analysis =
  case (op, args, numbers args) of
    (GSBeginText, [], _) -> case text analysis of
      Nothing -> Just (resetText (Just identity) (Just identity) analysis)
      Just _  -> Nothing

    (GSEndText, [], _) -> case text analysis of
      Nothing -> Nothing
      Just _  -> Just analysis{text = Nothing, deleted = commitText analysis}

    (GSSetTextMatrix, _, Just ns) -> do
      m <- matrixOf ns
      case text analysis of
        Nothing -> Nothing
        Just _  -> Just (resetText (Just m) (Just m) analysis)

    (GSMoveToNextLine, _, Just [x, y]) ->
      moveLine x y analysis

    (GSMoveToNextLineLP, _, Just [x, y]) ->
      moveLine x y analysis{graphics = state{leading = -y}}

    (GSNextLine, [], _) ->
      moveLine 0 (-leading state) analysis

    (GSSetTextFont, [GFXName name, GFXNumber fontSize], _)
      | finite fontSize ->
      update state{font = Map.lookup name fonts, size = toRational fontSize}

    (GSSetCharacterSpacing, _, Just [n]) ->
      update state{spacing = n}

    (GSSetWordSpacing, _, Just [n]) ->
      update state{wordSpacing = n}

    (GSSetHorizontalScaling, _, Just [n]) ->
      update state{horizontal = n / 100}

    (GSSetTextLeading, _, Just [n]) ->
      update state{leading = n}

    (GSSetTextRise, _, Just [n]) ->
      update state{rise = n}

    (GSSetTextRenderingMode, _, Just [n]) ->
      update state{rendering = n}

    (GSShowText, [value], _) ->
      showText viewport index [value] analysis

    (GSShowManyText, [GFXArray values], _) ->
      showText viewport index (toList values) analysis

    (GSNLShowText, _, _) -> quote analysis

    (GSNLShowTextWithSpacing, [GFXNumber w, GFXNumber c, _], _)
      | all finite [w, c] ->
      quote analysis { graphics = state { wordSpacing = toRational w
                                        , spacing = toRational c
                                        }
                     }

    _anyOtherCase
      | op `elem` harmless -> Just analysis
      | otherwise -> Nothing
 where
  state :: State
  state = graphics analysis

  update :: State -> Maybe Analysis
  update newState = Just analysis{graphics = newState}

{- | Extend the current path bounding box with the supplied point coordinates.
Each point is transformed through the current CTM before being merged into the
path's conservative rectangle.
-}
addPoints :: Int -> [Rational] -> Analysis -> Analysis
addPoints index coords analysis =
  analysis{currentPath = Path combined (index : indices) clipping}
 where
  Path box indices clipping = currentPath analysis

  points :: [Rational] -> [Rect]
  points []             = []
  points (x : y : rest) = bounds (ctm (graphics analysis)) x y 0 0 : points rest
  points _              = []

  combined :: Maybe Rect
  combined = foldl' (\acc b -> Just (maybe b (`union` b) acc))
                    box
                    (points coords)

{- | Finalize a path by deciding whether its painted output lies entirely off
page. The path is dropped when it is invisible, and a clipping end may be
preserved to keep the surrounding structure coherent.
-}
paintPath :: Rect -> Int -> GSOperator -> Analysis -> Analysis
paintPath viewport index op analysis =
  analysis
    { currentPath = Path Nothing [] False
    , deleted =
        if invisible && not clipping
          then foldr IS.insert (deleted analysis) (index : indices)
          else deleted analysis
    , ended =
        if invisible && clipping
          then IS.insert index (ended analysis)
          else ended analysis
    }
 where
  Path box indices clipping = currentPath analysis
  painted =
    box >>= \b ->
      if op `elem` [GSFillPathNZWR, GSFillPathEOR, GSEndPath]
        then Just b
        else strokeBounds (graphics analysis) b
  invisible = maybe False (outside viewport) painted

{- | Mark all text show operations tracked by the current text position as
committed.

This prevents earlier off-page shows from being silently lost when a later text
positioning reset occurs.
-}
commitText :: Analysis -> IntSet
commitText analysis = case text analysis of
  Just (TextPosition _ _ candidates) ->
    foldr IS.insert (deleted analysis) candidates
  Nothing ->
    deleted analysis

{- | Reset the active text position and line matrix after a positioning command.
The previous text candidates are committed before the new tracking state begins.
-}
resetText :: Maybe Matrix -> Maybe Matrix -> Analysis -> Analysis
resetText position line analysis =
  analysis
    { text = Just (TextPosition position line [])
    , deleted = commitText analysis
    }

{- | Advance the current text line by the supplied relative offset.

This keeps text tracking consistent across BT/ET resets and line movement
operators.
-}
moveLine :: Rational -> Rational -> Analysis -> Maybe Analysis
moveLine x y analysis = do
  TextPosition _ line _ <- text analysis
  let moved = fmap (`affine` (1, 0, 0, 1, x, y)) line
  return (resetText moved moved analysis)

{- | Quote operators have spacing and positioning side effects. Keep them and
suspend text tracking until an explicit positioning reset.
-}
quote :: Analysis -> Maybe Analysis
quote analysis = do
  TextPosition _ line _ <- text analysis
  let moved = fmap (`affine` (1, 0, 0, 1, 0, -leading (graphics analysis))) line
  return (resetText Nothing moved analysis)

{- | Evaluate a text-show sequence and register any glyph rectangles that fall
outside the page viewport.

This handles both literal strings and TJ-array adjustments while preserving the
text matrix for subsequent operators.
-}
showText :: Rect -> Int -> [GFXObject] -> Analysis -> Maybe Analysis
showText viewport index values analysis = do
  TextPosition position line candidates <- text analysis
  let
    state = graphics analysis
    position' = case position >>= \m -> measure state m values of
      Nothing -> TextPosition Nothing line []
      Just (m, rectangles) ->
        let
          invisible = rendering state `elem` [0, 1, 2, 3]
                    && all (outside viewport) rectangles
        in
          TextPosition (Just m)
                       line
                       (if invisible then index : candidates else [])

  return analysis{text = Just position'}

{- | Operators that terminate a path and trigger a visibility check.
-}
pathEndings :: [GSOperator]
pathEndings =
  [ GSStrokePath
  , GSCloseStrokePath
  , GSFillPathNZWR
  , GSFillPathEOR
  , GSFillStrokePathNZWR
  , GSFillStrokePathEOR
  , GSCloseFillStrokeNZWR
  , GSCloseFillStrokeEOR
  , GSEndPath
  ]

{- | Graphics operators that do not affect the off-page analysis state.
-}
harmless :: [GSOperator]
harmless =
  [ GSSetLineCap
  , GSSetLineJoin
  , GSSetLineDashPattern
  , GSSetColourRenderingIntent
  , GSSetFlatnessTolerance
  , GSSetStrokeColorspace
  , GSSetStrokeColor
  , GSSetStrokeColorN
  , GSSetStrokeGrayColorspace
  , GSSetStrokeRGBColorspace
  , GSSetStrokeCMYKColorspace
  , GSSetNonStrokeColorspace
  , GSSetNonStrokeColor
  , GSSetNonStrokeColorN
  , GSSetNonStrokeGrayColorspace
  , GSSetNonStrokeRGBColorspace
  , GSSetNonStrokeCMYKColorspace
  , GSPaintShapeColourShading
  , GSBeginMarkedContentSequence
  , GSBeginMarkedContentSequencePL
  , GSEndMarkedContentSequence
  , GSMarkedContentPoint
  , GSMarkedContentPointPL
  , GSBeginCompatibilitySection
  , GSEndCompatibilitySection
  ]

{- | Measure the glyph bounds produced by a text-show sequence.

The function also accounts for TJ-array adjustments and returns the final text
matrix along with the rectangles that were painted.
-}
measure :: State -> Matrix -> [GFXObject] -> Maybe (Matrix, [Rect])
measure state start values = do
  metrics <- font state
  go metrics start values
 where
  go _ matrix [] = Just (matrix, [])
  go metrics matrix (value : rest) = do
    (next, rectangles) <- measureValue state metrics matrix value
    (final, remaining) <- go metrics next rest
    return (final, rectangles ++ remaining)

{- | Measure a single text object, whether it is a string, hex string, or a TJ
spacing adjustment.
-}
measureValue
  :: State
  -> FontInfo
  -> Matrix
  -> GFXObject
  -> Maybe (Matrix, [Rect])
measureValue state metrics matrix = \case
  GFXString bytes ->
    measureString state metrics matrix (BS.unpack bytes)

  GFXHexString bytes ->
    measureString state metrics matrix (BS.unpack (fromHexDigits bytes))

  GFXNumber n
    | finite n ->
        let
          advance = -(toRational n * size state * horizontal state / 1000)
        in
          Just (affine matrix (1, 0, 0, 1, advance, 0), [])

  _anyOtherCase ->
    Nothing

{- | Measure each glyph in a byte string in sequence and accumulate their
bounds.
-}
measureString
  :: State
  -> FontInfo
  -> Matrix
  -> [Word8]
  -> Maybe (Matrix, [Rect])
measureString _ _ matrix [] = Just (matrix, [])
measureString state metrics matrix (byte : rest) = do
  (next, rectangle) <- measureGlyph state metrics matrix byte
  (final, rectangles) <- measureString state metrics next rest
  return (final, rectangle : rectangles)

{- | Measure a single glyph by transforming the font descriptor box for its code
point.

The advance is returned alongside the painted rectangle so the text matrix can
be updated correctly.
-}
measureGlyph :: State -> FontInfo -> Matrix -> Word8 -> Maybe (Matrix, Rect)
measureGlyph state (FontInfo fontBox widths) matrix byte = do
  width <- Map.lookup (fromIntegral byte) widths
  let
    scale :: (Rational, Rational, Rational, Rational, Rational, Rational)
    scale = ( size state * horizontal state / 1000
            , 0
            , 0
            , size state / 1000
            , 0
            , rise state
            )

    rectangle :: Rect
    rectangle = transform (affine (ctm state) (affine matrix scale)) fontBox

    advance :: Rational
    advance =
      ( width * size state / 1000
          + spacing state
          + if byte == 32 then wordSpacing state else 0
      ) * horizontal state

  painted <-
    if rendering state `elem` [1, 2, 5, 6]
      then strokeBounds state rectangle
      else Just rectangle

  return (affine matrix (1, 0, 0, 1, advance, 0), painted)
