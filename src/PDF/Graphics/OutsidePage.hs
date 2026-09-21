{- | Remove painting whose conservative bounds lie strictly outside the page.
Bounds are in default user space; clipping paths and text positioning remain
effective even when their associated painting is discarded.
-}
module PDF.Graphics.OutsidePage (FontInfo (FontInfo), removeOutsidePage) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (foldl', toList)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
  ( GFXObject (GFXArray, GFXHexString, GFXInlineImage, GFXName, GFXNumber, GFXString)
  , GSOperator (GSBeginCompatibilitySection, GSBeginInlineImage, GSBeginMarkedContentSequence, GSBeginMarkedContentSequencePL, GSBeginText, GSCloseFillStrokeEOR, GSCloseFillStrokeNZWR, GSCloseStrokePath, GSCloseSubpath, GSCubicBezierCurve, GSCubicBezierCurve1To, GSCubicBezierCurve2To, GSEndCompatibilitySection, GSEndMarkedContentSequence, GSEndPath, GSEndText, GSFillPathEOR, GSFillPathNZWR, GSFillStrokePathEOR, GSFillStrokePathNZWR, GSIntersectClippingPathEOR, GSIntersectClippingPathNZWR, GSLineTo, GSMarkedContentPoint, GSMarkedContentPointPL, GSMoveTo, GSMoveToNextLine, GSMoveToNextLineLP, GSNLShowText, GSNLShowTextWithSpacing, GSNextLine, GSPaintShapeColourShading, GSPaintXObject, GSRectangle, GSRestoreGS, GSSaveGS, GSSetCTM, GSSetCharacterSpacing, GSSetColourRenderingIntent, GSSetFlatnessTolerance, GSSetHorizontalScaling, GSSetLineCap, GSSetLineDashPattern, GSSetLineJoin, GSSetLineWidth, GSSetMiterLimit, GSSetNonStrokeCMYKColorspace, GSSetNonStrokeColor, GSSetNonStrokeColorN, GSSetNonStrokeColorspace, GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetParameters, GSSetStrokeCMYKColorspace, GSSetStrokeColor, GSSetStrokeColorN, GSSetStrokeColorspace, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace, GSSetTextFont, GSSetTextLeading, GSSetTextMatrix, GSSetTextRenderingMode, GSSetTextRise, GSSetWordSpacing, GSShowManyText, GSShowText, GSStrokePath)
  )
import Data.PDF.Program (Program)
import Data.Sequence qualified as SQ
import Data.Word (Word8)

import PDF.Graphics.InvisibleImages
  (Matrix, Rect (Rect), affine, balancedMarkedContent, bounds)

import Util.Hex (fromHexDigits)

{- | Simple horizontal fonts with reliable descriptor bounds and explicit widths.
Type 3 and composite fonts are deliberately excluded by the resource reader.
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

identity :: Matrix
identity = (1, 0, 0, 1, 0, 0)

union :: Rect -> Rect -> Rect
union (Rect a b c d) (Rect e f g h) =
  Rect (min a e) (min b f) (max c g) (max d h)

outside :: Rect -> Rect -> Bool
outside (Rect a b c d) (Rect e f g h) = c < e || g < a || d < f || h < b

transform :: Matrix -> Rect -> Rect
transform matrix (Rect a b c d) = bounds matrix a b (c - a) (d - b)

{- | L1 row norms safely bound affine expansion, including square caps and
miter joins. Hairlines have device-dependent width, so keep their strokes.
-}
strokeBounds :: State -> Rect -> Maybe Rect
strokeBounds state (Rect a b c d) = do
  (width, miter) <- stroke state
  if width <= 0
    then Nothing
    else do
      let (u, v, w, x, _, _) = ctm state
          radius = width * max 2 miter
          dx = radius * (abs u + abs w)
          dy = radius * (abs v + abs x)
      return (Rect (a - dx) (b - dy) (c + dx) (d + dy))

{- | Convert a list of `GFXObject` numbers to `Rational` values, if possible.
Returns `Nothing` if any number is invalid.
-}
numbers :: [GFXObject] -> Maybe [Rational]
numbers = traverse $ \case
  GFXNumber n | not (isNaN n || isInfinite n) -> Just (toRational n)
  _anyOtherCase                               -> Nothing

{- | Convert a list of `Rational` values to a `Matrix`, if possible.
Returns `Nothing` if the list does not contain exactly six elements.
-}
matrixOf :: [Rational] -> Maybe Matrix
matrixOf [a, b, c, d, e, f] = Just (a, b, c, d, e, f)
matrixOf _                  = Nothing

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
  | not (balancedMarkedContent 0 (toList program)) = program
  | otherwise = fromMaybe program $ do
      (deleted, ended) <-
        walk
          0
          initial
          []
          (Path Nothing [] False)
          Nothing
          IS.empty
          IS.empty
          (toList program)
      return $
        SQ.fromList
          [ if IS.member i ended then Command GSEndPath mempty else command
          | (i, command) <- zip [0 ..] (toList program)
          , IS.notMember i deleted
          ]
 where
  initial :: State
  initial = State identity (Just (1, 10)) Nothing 0 0 0 1 0 0 0

  walk
    :: Int
    -> State
    -> [State]
    -> Path
    -> Maybe TextPosition
    -> IntSet
    -> IntSet
    -> [Command]
    -> Maybe (IntSet, IntSet)
  walk
    index
    state
    stack
    path@(Path box indices clipping)
    text
    deleted
    ended
    commands =
    let
      next
        :: State
        -> [State]
        -> Path
        -> Maybe TextPosition
        -> IntSet
        -> IntSet
        -> [Command]
        -> Maybe (IntSet, IntSet)
      next = walk (index + 1)

      keep :: [Command] -> Maybe (IntSet, IntSet)
      keep = next state stack path text deleted ended

      commit :: IntSet
      commit = case text of
        Just (TextPosition _ _ candidates) -> foldr IS.insert deleted candidates
        Nothing                            -> deleted

      reset
        :: Maybe Matrix
        -> Maybe Matrix
        -> [Command]
        -> Maybe (IntSet, IntSet)
      reset position line =
        next
          state
          stack
          path
          (Just (TextPosition position line []))
          commit
          ended

      moveLine
        :: Rational
        -> Rational
        -> State
        -> [Command]
        -> Maybe (IntSet, IntSet)
      moveLine x y newState = case text of
        Just (TextPosition _ line _) ->
          let
            moved = fmap (`affine` (1, 0, 0, 1, x, y)) line
           in
            next
              newState
              stack
              path
              (Just (TextPosition moved moved []))
              commit
              ended
        Nothing -> const Nothing

      addPoints :: [Rational] -> [Command] -> Maybe (IntSet, IntSet)
      addPoints coords =
        let
          points :: [Rational] -> [Rect]
          points []             = []
          points (x : y : rest) = bounds (ctm state) x y 0 0 : points rest
          points _              = []

          boxes :: [Rect]
          boxes = points coords

          combined :: Maybe Rect
          combined = foldl' (\acc b -> Just (maybe b (`union` b) acc))
                            box
                            boxes
         in
          next
            state
            stack
            (Path combined (index : indices) clipping)
            text
            deleted
            ended

      paint :: GSOperator -> [Command] -> Maybe (IntSet, IntSet)
      paint op rest =
        let
          painted :: Maybe Rect
          painted =
            box >>= \b ->
              if op `elem` [GSFillPathNZWR, GSFillPathEOR, GSEndPath]
                then Just b
                else strokeBounds state b

          invisible :: Bool
          invisible = maybe False (outside viewport) painted

          deleted' :: IntSet
          deleted' =
            if invisible && not clipping
              then foldr IS.insert deleted (index : indices)
              else deleted

          ended' :: IntSet
          ended' =
            if invisible && clipping
              then IS.insert index ended
              else ended
        in
          next state stack (Path Nothing [] False) text deleted' ended' rest

      showText :: [GFXObject] -> [Command] -> Maybe (IntSet, IntSet)
      showText values rest = case text of
        Nothing -> Nothing
        Just (TextPosition position line candidates) ->
          case position >>= \m -> measure state m values of
            Nothing ->
              next
                state
                stack
                path
                (Just (TextPosition Nothing line []))
                deleted
                ended
                rest
            Just (position', rectangles) ->
              let invisible =
                    rendering state `elem` [0, 1, 2, 3]
                      && all (outside viewport) rectangles
                  candidates' = if invisible then index : candidates else []
               in next
                    state
                    stack
                    path
                    (Just (TextPosition (Just position') line candidates'))
                    deleted
                    ended
                    rest
     in
      case commands of
        []
          | null stack && isNothing text && not clipping ->
            Just (deleted, ended)
          | otherwise -> Nothing

        Command op args : rest ->
          case (op, toList args, numbers (toList args)) of
            (GSSaveGS, [], _) ->
              next state (state : stack) path text deleted ended rest

            (GSRestoreGS, [], _) -> case stack of
              saved : more -> next saved more path text deleted ended rest
              []           -> Nothing

            (GSSetCTM, _, Just ns) -> do
              m <- matrixOf ns
              next
                (state{ctm = affine (ctm state) m})
                stack
                path
                text
                deleted
                ended
                rest

            (GSSetLineWidth, _, Just [w]) | w >= 0 ->
              next
                (state{stroke = fmap (\(_, m) -> (w, m)) (stroke state)})
                stack
                path
                text
                deleted
                ended
                rest

            (GSSetMiterLimit, _, Just [m]) | m >= 1 ->
              next
                (state{stroke = fmap (\(w, _) -> (w, m)) (stroke state)})
                stack
                path
                text
                deleted
                ended
                rest

            (GSSetParameters, _, _) ->
              -- ExtGState can change stroke parameters and the selected font.
              next
                (state{stroke = Nothing, font = Nothing})
                stack
                path
                text
                deleted
                ended
                rest

            (GSRectangle, _, Just [x, y, w, h]) ->
              addPoints [x, y, x + w, y, x, y + h, x + w, y + h] rest

            (GSMoveTo, _, Just [x, y]) ->
              addPoints [x, y] rest

            (GSLineTo, _, Just [x, y]) | not (null indices) ->
              addPoints [x, y] rest

            (GSCubicBezierCurve, _, Just ns)
              | length ns == 6 && not (null indices) ->
              addPoints ns rest

            (GSCubicBezierCurve1To, _, Just ns)
              | length ns == 4 && not (null indices) ->
              addPoints ns rest

            (GSCubicBezierCurve2To, _, Just ns)
              | length ns == 4 && not (null indices) ->
              addPoints ns rest

            (GSCloseSubpath, [], _) | not (null indices) ->
              next
                state
                stack
                (Path box (index : indices) clipping)
                text
                deleted
                ended
                rest

            (GSIntersectClippingPathNZWR, [], _) ->
              next state stack (Path box indices True) text deleted ended rest

            (GSIntersectClippingPathEOR, [], _) ->
              next state stack (Path box indices True) text deleted ended rest

            (_, [], _)
              | op
                  `elem` [ GSStrokePath
                        , GSCloseStrokePath
                        , GSFillPathNZWR
                        , GSFillPathEOR
                        , GSFillStrokePathNZWR
                        , GSFillStrokePathEOR
                        , GSCloseFillStrokeNZWR
                        , GSCloseFillStrokeEOR
                        , GSEndPath
                        ] ->
                  paint op rest

            (GSPaintXObject, [GFXName name], _) ->
              let
                off =
                  maybe False
                        (outside viewport . transform (ctm state))
                        (Map.lookup name objects)
              in
                next
                  state
                  stack
                  path
                  text
                  (if off then IS.insert index deleted else deleted)
                  ended
                  rest

            (GSBeginInlineImage, [GFXInlineImage _ _], _) ->
              let
                off = outside viewport (bounds (ctm state) 0 0 1 1)
              in
                next
                  state
                  stack
                  path
                  text
                  (if off then IS.insert index deleted else deleted)
                  ended
                  rest

            (GSBeginText, [], _) -> case text of
              Nothing -> reset (Just identity) (Just identity) rest
              Just _  -> Nothing

            (GSEndText, [], _) -> case text of
              Nothing -> Nothing
              Just _  -> next state stack path Nothing commit ended rest

            (GSSetTextMatrix, _, Just ns) -> do
              m <- matrixOf ns
              case text of
                Nothing -> Nothing
                Just _  -> reset (Just m) (Just m) rest

            (GSMoveToNextLine, _, Just [x, y]) ->
              moveLine x y state rest

            (GSMoveToNextLineLP, _, Just [x, y]) ->
              moveLine x y (state{leading = -y}) rest

            (GSNextLine, [], _) ->
              moveLine 0 (-leading state) state rest

            (GSSetTextFont, [GFXName name, GFXNumber s], _)
              | not (isNaN s || isInfinite s) ->
              next
                (state{font = Map.lookup name fonts, size = toRational s})
                stack
                path
                text
                deleted
                ended
                rest

            (GSSetCharacterSpacing, _, Just [n]) ->
              next (state{spacing = n}) stack path text deleted ended rest

            (GSSetWordSpacing, _, Just [n]) ->
              next (state{wordSpacing = n}) stack path text deleted ended rest

            (GSSetHorizontalScaling, _, Just [n]) ->
              next
                (state{horizontal = n / 100})
                stack
                path
                text
                deleted
                ended
                rest

            (GSSetTextLeading, _, Just [n]) ->
              next (state{leading = n}) stack path text deleted ended rest

            (GSSetTextRise, _, Just [n]) ->
              next (state{rise = n}) stack path text deleted ended rest

            (GSSetTextRenderingMode, _, Just [n]) ->
              next (state{rendering = n}) stack path text deleted ended rest

            (GSShowText, [s], _) -> showText [s] rest

            (GSShowManyText, [GFXArray values], _) ->
              showText (toList values) rest

            -- Quote operators reset the line and may change spacing. Preserve
            -- them and stop tracking text until a subsequent explicit
            -- positioning reset.
            (GSNLShowText, _, _) ->
              quote state rest

            (GSNLShowTextWithSpacing, [GFXNumber w, GFXNumber c, _], _)
              | all finite [w, c] ->
              quote (state{wordSpacing = toRational w, spacing = toRational c})
                    rest

            _anyOtherCase
              | op `elem` harmless -> keep rest
              | otherwise          -> Nothing
   where
    finite :: Double -> Bool
    finite n = not (isNaN n || isInfinite n)
    quote newState rest = case text of
      Just (TextPosition _ line candidates) ->
        let moved = fmap (`affine` (1, 0, 0, 1, 0, -leading newState)) line
         in walk
              (index + 1)
              newState
              stack
              path
              (Just (TextPosition Nothing moved []))
              (foldr IS.insert deleted candidates)
              ended
              rest
      Nothing -> Nothing

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

{- | Each glyph is bounded by the font-wide box, not by its advance width.
Bezier control points similarly provide an outer bound for path curves.
-}
measure :: State -> Matrix -> [GFXObject] -> Maybe (Matrix, [Rect])
measure state start values = do
  FontInfo fontBox widths <- font state
  let
    glyph m byte = do
      width <- Map.lookup (fromIntegral byte) widths

      let
        scale :: Matrix
        scale = ( size state * horizontal state / 1000
                , 0
                , 0
                , size state / 1000
                , 0
                , rise state
                )

        rectangle :: Rect
        rectangle = transform (affine (ctm state) (affine m scale)) fontBox

        advance :: Rational
        advance =
          ( width * size state / 1000
              + spacing state
              + if byte == 32 then wordSpacing state else 0
          )
            * horizontal state

      painted <-
        if rendering state `elem` [1, 2, 5, 6]
          then strokeBounds state rectangle
          else Just rectangle

      return (affine m (1, 0, 0, 1, advance, 0), painted)

    string :: Matrix -> [Word8] -> Maybe (Matrix, [Rect])
    string m [] = Just (m, [])
    string m (b : bs) = do
      (m', rectangle) <- glyph m b
      (m'', rectangles) <- string m' bs
      return (m'', rectangle : rectangles)

    go :: Matrix -> [GFXObject] -> Maybe (Matrix, [Rect])
    go m [] = Just (m, [])
    go m (value : rest) = do
      (m', rectangles) <- case value of
        GFXString bytes -> string m (BS.unpack bytes)

        GFXHexString bytes -> string m (BS.unpack (fromHexDigits bytes))

        GFXNumber n | not (isNaN n || isInfinite n) ->
          Just
                (affine
                  m
                  ( 1
                  , 0
                  , 0
                  , 1
                  ,- (toRational n * size state * horizontal state / 1000)
                  , 0
                  )
                , []
                )

        _anyOtherCase -> Nothing

      (m'', remaining) <- go m' rest

      return (m'', rectangles ++ remaining)

  go start values
