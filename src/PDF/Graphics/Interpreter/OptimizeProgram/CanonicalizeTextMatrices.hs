-- | Exact text positioning after the precision-reducing passes have finished.
-- Keep the glyph matrix and line matrix separate. Only accept a rewrite when
-- its serialized operands reproduce the required state exactly.
module PDF.Graphics.Interpreter.OptimizeProgram.CanonicalizeTextMatrices
  ( canonicalizeTextMatrices
  ) where

import Control.Monad (foldM)

import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.List (minimumBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust)
import Data.Ord (comparing)
import Data.PDF.Command (Command (Command, cOperator, cParameters), mkCommand)
import Data.PDF.GFXObject
  ( GFXObject (GFXArray, GFXName, GFXNumber, GFXString)
  , GSOperator (GSBeginText, GSEndText, GSMoveToNextLine, GSMoveToNextLineLP, GSNLShowText, GSNLShowTextWithSpacing, GSNextLine, GSPaintXObject, GSRestoreGS, GSSaveGS, GSSetCharacterSpacing, GSSetHorizontalScaling, GSSetParameters, GSSetTextFont, GSSetTextLeading, GSSetTextMatrix, GSSetWordSpacing, GSShowManyText, GSShowText, GSUnknown)
  , separateGfx
  )
import Data.PDF.Program (Program, extractObjects)
import Data.Sequence qualified as SQ

import PDF.Graphics.Geometry (Matrix, affine, identity, matrixOf)
import PDF.Graphics.Interpreter.OptimizeParameters (convertHexString)
import PDF.Graphics.TextMetrics
  ( ExtFont (SelectedFont, UnchangedFont)
  , FontMetrics
  , TextResources (textExtFonts, textFonts)
  , glyphWidths
  , serializedNumber
  )

-- Text parameters belong to the graphics stack; text-object matrices do not
-- share their default values or lifetime. Unknown inherited values are
-- explicit.
type Parameters :: Type
data Parameters = Parameters
  { characterSpacing :: !(Maybe Rational)
  , wordSpacing      :: !(Maybe Rational)
  , horizontalScale  :: !(Maybe Rational)
  , leading          :: !(Maybe Rational)
  , font             :: !(Maybe (FontMetrics, Rational))
  } deriving stock (Eq, Show)

type TextPosition :: Type
data TextPosition = TextPosition
  { textMatrix :: !(Maybe Matrix)
  , lineMatrix :: !(Maybe Matrix)
  , parameters :: !Parameters
  , stack      :: ![Parameters]
  , insideText :: !Bool
  } deriving stock (Eq, Show)

unknownParameters :: Parameters
unknownParameters = Parameters Nothing Nothing Nothing Nothing Nothing

initial :: Bool -> TextPosition
initial inherited = TextPosition Nothing Nothing params [] False
 where
  params = if inherited
            then unknownParameters
            else Parameters (Just 0) (Just 0) (Just 1) (Just 0) Nothing

number :: GFXObject -> Maybe Rational
number (GFXNumber n) = serializedNumber n
number _             = Nothing

numbers :: Command -> Maybe [Rational]
numbers = traverse number . toList . cParameters

move :: Rational -> Rational -> TextPosition -> TextPosition
move x y state = state { textMatrix = matrix, lineMatrix = matrix }
 where
  matrix = (\m -> affine m (1, 0, 0, 1, x, y)) <$> lineMatrix state

nextLine :: TextPosition -> TextPosition
nextLine state = case leading (parameters state) of
  Just l  -> move 0 (-l) state
  Nothing -> unknownPosition state

unknownPosition :: TextPosition -> TextPosition
unknownPosition state = state { textMatrix = Nothing, lineMatrix = Nothing }

-- | Each glyph, including the last, contributes character spacing. Word spacing
-- applies only to a single-byte code 32. TJ numbers translate Tm, never Tlm.
showItems :: [GFXObject] -> TextPosition -> TextPosition
showItems items state = state { textMatrix = textMatrix state >>= advance }
 where
  params :: Parameters
  params = parameters state

  advance :: Matrix -> Maybe Matrix
  advance matrix = foldM showItem matrix items

  showItem :: Matrix -> GFXObject -> Maybe Matrix
  showItem matrix object = case convertHexString object of
    GFXString bytes | BS.null bytes -> Just matrix

    GFXString bytes -> do
      (metrics, fontSize) <- font params
      widths <- glyphWidths metrics bytes
      tc <- characterSpacing params
      hz <- horizontalScale params

      let
        glyph :: Rational -> (Rational, Bool) -> Maybe Rational
        glyph total (width, space) = do
          tw <- if space
                  then wordSpacing params
                  else Just 0

          pure (total + (width * fontSize / 1000 + tc + tw) * hz)

      distance <- foldM glyph 0 widths
      pure (affine matrix (1, 0, 0, 1, distance, 0))

    GFXNumber n -> do
      adjustment <- serializedNumber n
      -- A known supported font also proves horizontal writing mode.
      (_, fontSize) <- font params
      hz <- horizontalScale params
      pure (affine matrix (1, 0, 0, 1,- (adjustment * fontSize * hz / 1000), 0))

    _ -> Nothing

showTextString :: GFXObject -> TextPosition -> TextPosition
showTextString object state = case convertHexString object of
  text@GFXString{} -> showItems [text] state
  _                -> unknownPosition state

-- | Interpret the commands actually emitted by the preceding passes. No state
-- is inferred from rounded-away operands or from a Unicode character mapping.
interpret :: TextResources -> TextPosition -> Command -> TextPosition
interpret resources state command@(Command op operands) = case (op, toList operands) of
  (GSBeginText, []) ->
    state { textMatrix = Just identity
          , lineMatrix = Just identity
          , insideText = True
          }

  (GSEndText, []) -> state { insideText = False }

  (GSSaveGS, []) -> state { stack = params : stack state }

  (GSRestoreGS, []) ->
    let
      restored :: TextPosition
      restored = case stack state of
          saved : rest -> state { parameters = saved, stack = rest }
          []           -> state { parameters = unknownParameters }

    -- PDF revisions/readers differ on q/Q within BT/ET. Do not prove text
    -- rewrites across that boundary under either matrix-restoration rule.
    in
      if insideText state
        then unknownPosition restored
        else restored

  (GSSetTextFont, [GFXName name, size]) -> set $ params
    { font = (,) <$> Map.lookup name (textFonts resources) <*> number size }

  (GSSetParameters, [GFXName name]) ->
    set $ params { font = case Map.lookup name (textExtFonts resources) of
      Just UnchangedFont               -> font params
      Just (SelectedFont metrics size) -> Just (metrics, size)
      _                                -> Nothing }

  (GSSetCharacterSpacing, [n]) ->
    set $ params { characterSpacing = number n }

  (GSSetWordSpacing, [n]) ->
    set $ params { wordSpacing = number n }

  (GSSetHorizontalScaling, [n]) ->
    set $ params { horizontalScale = (/ 100) <$> number n }

  (GSSetTextLeading, [n]) -> set $ params { leading = number n }

  (GSSetTextMatrix, _) | insideText state ->
    let
      matrix :: Maybe Matrix
      matrix = numbers command >>= matrixOf
    in
      state { textMatrix = matrix, lineMatrix = matrix }

  (GSMoveToNextLine, _) | insideText state -> case numbers command of
    Just [x, y] -> move x y state
    _           -> unknownPosition state

  (GSMoveToNextLineLP, _) | insideText state -> case numbers command of
    Just [x, y] -> move x y (set $ params { leading = Just (-y) })
    _           -> unknownPosition (set $ params { leading = Nothing })

  (GSNextLine, []) | insideText state -> nextLine state

  (GSShowText, [text]) -> showTextString text state

  (GSShowManyText, [GFXArray items]) -> showItems (toList items) state

  (GSNLShowText, [text]) -> showTextString text (nextLine state)

  (GSNLShowTextWithSpacing, [word, char, text]) ->
    showTextString text (nextLine (set $ params
      { wordSpacing = number word, characterSpacing = number char }))

  -- Unknown and malformed text/state operators are barriers. A Form invocation
  -- may contain text objects, so its text matrices cannot be assumed unchanged.
  (GSUnknown _, _) -> invalidate

  (GSPaintXObject, _) -> unknownPosition state

  _ | op `elem` [ GSSetTextFont
                , GSSetParameters
                , GSSetCharacterSpacing
                , GSSetWordSpacing
                , GSSetHorizontalScaling
                , GSSetTextLeading
                , GSShowText
                , GSShowManyText
                , GSNLShowText
                , GSNLShowTextWithSpacing
                , GSBeginText
                , GSEndText
                , GSSaveGS
                , GSRestoreGS
                , GSNextLine
                ] -> invalidate
    | otherwise -> state
 where
  params :: Parameters
  params = parameters state

  set :: Parameters -> TextPosition
  set p = state { parameters = p }

  invalidate :: TextPosition
  invalidate = unknownPosition (set unknownParameters)

positioning :: Command -> Bool
positioning command = cOperator command `elem`
  [ GSSetTextMatrix
  , GSMoveToNextLine
  , GSMoveToNextLineLP
  , GSNextLine
  ]

-- | Backward liveness in one traversal. A redundant Tm can be removed after a
-- show only if its new line origin will not be read before an absolute reset.
-- q/Q, Form calls and unknown operators are conservatively treated as readers.
lineNeededBefore :: Command -> Bool -> Bool
lineNeededBefore command after = case cOperator command of
  GSSetTextMatrix | Just ns <- numbers command, length ns == 6 -> False

  GSBeginText | null (cParameters command) -> False

  GSEndText | null (cParameters command) -> False

  op | op `elem` [ GSMoveToNextLine
                 , GSMoveToNextLineLP
                 , GSNextLine
                 , GSNLShowText
                 , GSNLShowTextWithSpacing
                 , GSSaveGS
                 , GSRestoreGS
                 , GSPaintXObject
                 ] -> True

  GSUnknown _ -> True

  _ -> after

-- | Build a candidate only when every decimal survives serialization exactly.
-- Inverse matrices can yield repeating fractions; those candidates are
-- rejected.
makeCommand :: GSOperator -> [Rational] -> Maybe Command
makeCommand op ns = do
  operands <- traverse exact ns
  pure (mkCommand op operands)
 where
  exact :: Rational -> Maybe GFXObject
  exact n =
    let
      d :: Double
      d = fromRational n
    in
      if serializedNumber d == Just n
        then Just (GFXNumber d)
        else Nothing

candidates :: TextPosition -> TextPosition -> [Command]
candidates before target = catMaybes $ case lineMatrix target of
  Nothing -> []

  Just (a, b, c, d, e, f) ->
    [ makeCommand GSSetTextMatrix [a, b, c, d, e, f]
    , Just (mkCommand GSNextLine [])
    ]
    ++ [ makeCommand GSSetTextLeading [l]
       | Just l <- [leading (parameters target)]
       ]
    ++ case lineMatrix before of
      Just (u, v, w, z, x0, y0) | u * z - v * w /= 0 ->
        let
          determinant :: Rational
          determinant = u * z - v * w

          x :: Rational
          x = (z * (e - x0) - w * (f - y0)) / determinant

          y :: Rational
          y = (u * (f - y0) - v * (e - x0)) / determinant
        in
          [ makeCommand op [x, y]
          | op <- [GSMoveToNextLine, GSMoveToNextLineLP]
          ]

      _ -> []

samePosition :: Bool -> TextPosition -> TextPosition -> Bool
samePosition lineLive actual target =
  textMatrix actual == textMatrix target
  && (not lineLive || lineMatrix actual == lineMatrix target)
  && leading (parameters actual) == leading (parameters target)

cost :: [Command] -> Int
cost = BS.length . separateGfx . extractObjects . SQ.fromList

-- | The Bool is True for inherited Form state, False for page defaults. This
-- pass runs after precision reduction, and before graphics-state factorization;
-- it never rounds a synthesized relative displacement on a later iteration.
canonicalizeTextMatrices :: TextResources -> Bool -> Program -> Program
canonicalizeTextMatrices resources inherited program =
  SQ.fromList (go (initial inherited) annotated)
 where
  annotated :: [(Command, Bool)]
  annotated = snd $ foldr annotate (False, []) (toList program)

  annotate :: Command -> (Bool, [(Command, Bool)]) -> (Bool, [(Command, Bool)])
  annotate command (live, rest) =
    (lineNeededBefore command live, (command, live) : rest)

  go :: TextPosition -> [(Command, Bool)] -> [Command]
  go _ [] = []
  go state ((command, lineLive) : rest) =
    let
      target :: TextPosition
      target = interpret resources state command

      options :: [[Command]]
      options = [ [candidate] | candidate <- candidates state target ]

      valid :: [Command] -> Bool
      valid cmds = samePosition
                    lineLive
                    (foldl (interpret resources) state cmds)
                    target

      known :: Bool
      known = insideText state && isJust (textMatrix target)

      alternatives :: [[Command]]
      alternatives = if positioning command && known
                      then filter valid ([] : options)
                      else []

      chosen :: [Command]
      chosen = minimumBy (comparing cost) ([command] : alternatives)

      emitted :: TextPosition
      emitted = foldl (interpret resources) state chosen
    in
      chosen ++ go emitted rest
