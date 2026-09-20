{-| Conservative text positioning: only adjacent positioning commands are
combined. Glyph advances are unknown; the text-line origin remains known. -}
module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeTextMatrix
  ( optimizeTextMatrix
  ) where

import Control.Monad.State (State, gets)
import Data.ByteString qualified as BS
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.PDF.Command (Command (Command, cOperator, cParameters), mkCommand)
import Data.PDF.GFXObject (GFXObject (GFXNumber), GSOperator (GSSetTextMatrix, GSMoveToNextLine, GSMoveToNextLineLP, GSNextLine, GSBeginText, GSSetTextLeading, GSNLShowText, GSNLShowTextWithSpacing, GSShowText, GSShowManyText), separateGfx)
import Data.PDF.GraphicsState (GraphicsState (gsTextState))
import Data.PDF.InterpreterAction (InterpreterAction (ReplaceCommand, ReplaceAndDeleteNextCommand, KeepCommand, DeleteCommand), replaceCommandWith)
import Data.PDF.InterpreterState
  ( InterpreterState (iGraphicsState), modifyGraphicsStateS, resetTextStateS
  , setTextLeadingS, setTextMatrixS, setCharacterSpacingS, setWordSpacingS
  , applyTextMatrixS, usefulTextPrecisionS )
import Data.PDF.Program (Program, extractObjects)
import Data.PDF.TextState (TextState (tsMatrix, tsLineMatrix, tsLeading))
import Data.PDF.TransformationMatrix (TransformationMatrix (TransformationMatrix, tmA, tmB, tmC, tmD, tmE, tmF))
import Data.Sequence (Seq (Empty, (:<|)))
import Data.Sequence qualified as SQ
import PDF.Graphics.Interpreter.OptimizeParameters (optimizeParameters)

-- Interpret positioning without requiring glyph widths.
position :: Command -> (TransformationMatrix, Double) -> Maybe (TransformationMatrix, Double)
position (Command op ps) (matrix, leading) = case (op, ps) of
  (GSSetTextMatrix, GFXNumber a :<| GFXNumber b :<| GFXNumber c :<| GFXNumber d :<| GFXNumber e :<| GFXNumber f :<| Empty) ->
    Just (TransformationMatrix a b c d e f, leading)
  (GSMoveToNextLine, GFXNumber x :<| GFXNumber y :<| Empty) -> move x y leading
  (GSMoveToNextLineLP, GFXNumber x :<| GFXNumber y :<| Empty) -> move x y (-y)
  (GSNextLine, Empty) -> move 0 (-leading) leading
  _ -> Nothing
 where
  move x y l = Just (matrix <> TransformationMatrix 1 0 0 1 x y, l)

size :: [Command] -> Int
size = BS.length . separateGfx . extractObjects . SQ.fromList

-- Candidates must reproduce both the line origin and leading exactly. Inverting
-- the linear part supports rotations and shear; singular matrices use Tm only.
encodings :: (TransformationMatrix, Double) -> (TransformationMatrix, Double) -> [Command]
encodings initial@(old, _) target@(m, leading) =
  filter (\cmd -> position cmd initial == Just target) candidates
 where
  command op = mkCommand op . map GFXNumber
  absolute = command GSSetTextMatrix [tmA m, tmB m, tmC m, tmD m, tmE m, tmF m]
  determinant = tmA old * tmD old - tmB old * tmC old
  dx = tmE m - tmE old
  dy = tmF m - tmF old
  x = (tmD old * dx - tmC old * dy) / determinant
  y = (tmA old * dy - tmB old * dx) / determinant
  candidates = absolute : mkCommand GSNextLine [] :
    [command op [x, y] | determinant /= 0, op <- [GSMoveToNextLine, GSMoveToNextLineLP]] ++
    [command GSMoveToNextLineLP [0, -leading]]

optimizeTextMatrix :: Command -> Program -> State InterpreterState InterpreterAction
optimizeTextMatrix command rest = do
  ts <- gets (gsTextState . iGraphicsState)
  let initial = (tsLineMatrix ts, tsLeading ts)
  case position command initial of
    Just target -> case rest of
      next :<| _ | Just final <- position next target -> do
        let candidates = encodings initial final
        case candidates of
          _ : _ | let best = minimumBy (comparing (size . (:[]))) candidates
                , size [best] < size [command, next] -> do
                  applyPosition final
                  return (ReplaceAndDeleteNextCommand best)
          _ -> preserveLeading target next
      _ -> keepPosition
    Nothing -> case (cOperator command, cParameters command) of
      (GSBeginText, Empty) -> resetTextStateS >> return KeepCommand
      (GSSetTextLeading, GFXNumber _leading :<| Empty) -> do
        precision <- usefulTextPrecisionS
        let emitted = optimizeParameters command precision
        case cParameters emitted of
          GFXNumber leading :<| Empty -> setTextLeadingS leading
          _ -> pure ()
        return (ReplaceCommand emitted)
      (GSNLShowText, _) -> nextLine >> unknown >> return KeepCommand
      (GSNLShowTextWithSpacing, GFXNumber word :<| GFXNumber char :<| _text :<| Empty) -> do
        setWordSpacingS word
        setCharacterSpacingS char
        nextLine
        unknown
        return KeepCommand
      (GSShowText, _) -> unknown >> return KeepCommand
      (GSShowManyText, _) -> unknown >> return KeepCommand
      _ -> return KeepCommand
 where
  -- Track the actual emitted operands so subsequent rewrites see the same
  -- line origin as a PDF reader, including after precision reduction.
  keepPosition = do
    precision <- usefulTextPrecisionS
    ts <- gets (gsTextState . iGraphicsState)
    let emitted = optimizeParameters command precision
    case position emitted (tsLineMatrix ts, tsLeading ts) of
      Just target -> applyPosition target
      Nothing -> pure ()
    return (ReplaceCommand emitted)
  applyPosition (matrix, leading) = setTextMatrixS matrix >> setTextLeadingS leading
  unknown = modifyGraphicsStateS $ \gs -> gs { gsTextState = (gsTextState gs) { tsMatrix = Nothing } }
  nextLine = do
    leading <- gets (tsLeading . gsTextState . iGraphicsState)
    applyTextMatrixS (TransformationMatrix 1 0 0 1 0 (-leading))
  preserveLeading target next
    | cOperator next == GSSetTextMatrix =
        if cOperator command == GSMoveToNextLineLP
          then do
            setTextLeadingS (snd target)
            return $ replaceCommandWith command (mkCommand GSSetTextLeading [GFXNumber (snd target)])
          else return DeleteCommand
    | otherwise = keepPosition
