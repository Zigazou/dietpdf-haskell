module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeTextMatrixSpec (spec) where

import Control.Monad (foldM, forM_, replicateM)
import Control.Monad.State (State, execState)
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.PDF.Command (Command (cOperator, cParameters), mkCommand)
import Data.PDF.GFXObject
  ( GFXObject (GFXNumber)
  , GSOperator
    ( GSBeginText, GSEndText, GSMoveToNextLine, GSMoveToNextLineLP
    , GSNextLine, GSSetTextLeading, GSSetTextMatrix
    )
  )
import Data.PDF.GraphicsState (gsTextState)
import Data.PDF.InterpreterState (InterpreterState, defaultInterpreterState, iGraphicsState, setTextMatrixS, saveStateS, restoreStateS)
import Data.PDF.Program (Program, mkProgram, parseProgram, programComputedSize)
import Data.PDF.TextState (TextState (tsMatrix, tsLineMatrix, tsLeading, tsCharacterSpacing, tsWordSpacing))
import Data.PDF.TransformationMatrix (TransformationMatrix (TransformationMatrix))
import Data.PDF.WorkData (emptyWorkData)
import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeTextMatrix (optimizeTextMatrix)
import PDF.Graphics.Interpreter.OptimizeProgram (optimizeProgram)
import PDF.Graphics.Parser.Stream (gfxParse)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, choose, elements, forAll, property)

parse :: ByteString -> Program
parse input = either (error . show) parseProgram (gfxParse input)

stateAfter :: ByteString -> TextState
stateAfter input = gsTextState . iGraphicsState $
  execState (foldM step () (parse input)) defaultInterpreterState
 where
  step :: () -> Command -> State InterpreterState ()
  step () command = optimizeTextMatrix command mempty >> pure ()

spec :: Spec
spec = do
  describe "text positioning state" $ do
    it "invalidates only the glyph matrix after showing text" $ do
      let ts = stateAfter "BT 10 20 Td (hello) Tj"
      tsMatrix ts `shouldBe` Nothing
      tsLineMatrix ts `shouldBe` TransformationMatrix 1 0 0 1 10 20
    it "uses the line origin for Td after text" $ do
      tsMatrix (stateAfter "BT 10 20 Td (hello) Tj 3 4 Td")
        `shouldBe` Just (TransformationMatrix 1 0 0 1 13 24)
    it "handles rotated line origins" $ do
      tsLineMatrix (stateAfter "BT 0 1 -1 0 10 20 Tm (hello) Tj 3 4 Td")
        `shouldBe` TransformationMatrix 0 1 (-1) 0 6 23
    it "tracks leading through TD, T*, and both quote operators" $ do
      let ts = stateAfter "BT 0 -12 TD (a) Tj T* (b) ' 2 3 (c) \""
      tsLineMatrix ts `shouldBe` TransformationMatrix 1 0 0 1 0 (-48)
      tsMatrix ts `shouldBe` Nothing
      tsLeading ts `shouldBe` 12
      tsWordSpacing ts `shouldBe` 2
      tsCharacterSpacing ts `shouldBe` 3
    it "resets both matrices at BT while retaining leading" $ do
      let ts = stateAfter "BT 10 -12 TD (a) Tj ET BT"
      tsMatrix ts `shouldBe` Just mempty
      tsLineMatrix ts `shouldBe` mempty
      tsLeading ts `shouldBe` 12
    it "does not restore text matrices with Q" $ do
      let matrix = TransformationMatrix 1 0 0 1 30 40
          ts = gsTextState . iGraphicsState $ execState
            (saveStateS >> setTextMatrixS matrix >> restoreStateS)
            defaultInterpreterState
      tsMatrix ts `shouldBe` Just matrix
      tsLineMatrix ts `shouldBe` matrix
  describe "adjacent text positioning canonicalization" $ do
    forM_
      [ ("BT 10 20 Td 3 4 Td (x) Tj ET", "BT 13 24 Td (x) Tj ET")
      , ("BT 1 0 0 1 10 20 Tm 3 4 Td (x) Tj ET", "BT 13 24 Td (x) Tj ET")
      , ("BT 0 1 -1 0 10 20 Tm 3 4 Td (x) Tj ET", "BT 0 1 -1 0 6 23 Tm (x) Tj ET")
      , ("BT 10 20 Td 1 0 0 1 30 40 Tm (x) Tj ET", "BT 30 40 Td (x) Tj ET")
      , ("BT 10 -12 TD 1 0 0 1 30 40 Tm (x) Tj T* (y) Tj ET", "BT 12 TL 1 0 0 1 30 40 Tm (x) Tj T* (y) Tj ET")
      , ("BT 12 TL 0 0 Td 0 -12 Td (x) Tj ET", "BT 12 TL T* (x) Tj ET")
      , ("BT 0 0 0 0 10 20 Tm 3 4 Td (x) Tj ET", "BT 0 0 0 0 10 20 Tm (x) Tj ET")
      -- Multiple consecutive Tm: only the last one that is actually observed matters.
      , ("BT 1 0 0 1 10 20 Tm 0 1 -1 0 30 40 Tm (x) Tj ET", "BT 0 1 -1 0 30 40 Tm (x) Tj ET")
      -- Pure scale (no rotation/skew): collapses when the net effect is a translation-only Td/T*.
      , ("BT 2 0 0 3 10 20 Tm 0 0 Td (x) Tj ET", "BT 2 0 0 3 10 20 Tm (x) Tj ET")
      -- Pure skew: proving equivalence requires the full matrix, not just e/f.
      , ("BT 1 0 0.5 1 10 20 Tm 3 4 Td (x) Tj ET", "BT 1 0 0.5 1 15 24 Tm (x) Tj ET")
      -- Negative, fractional and mixed-notation numbers.
      , ("BT -10.25 -0.125 Td 0.125 4.375 Td (x) Tj ET", "BT -10.125 4.25 Td (x) Tj ET")
      -- TJ (array show) also acts as a barrier for the Tm that follows it.
      , ("BT 10 20 Td [(x)] TJ ET", "BT 10 20 Td (x) Tj ET")
      ] $ \(input, expected) -> it (show input) $
        optimizeProgram emptyWorkData (parse input) `shouldBe` optimizeProgram emptyWorkData (parse expected)
    forM_
      [ "BT 1 0 0 1 10 20 Tm (x) Tj 1 0 0 1 10 20 Tm (y) Tj ET"
      , "BT 10 20 Td (x) Tj 3 4 Td (y) Tj ET"
      , "BT 10 20 Td (x) Tj ET BT 10 20 Td (y) Tj ET"
      , "BT 0 -12 TD 0 -12 TD (x) Tj T* (y) Tj ET"
      , "BT 0 -12 TD 0 -8 Td (x) Tj T* (y) Tj ET"
      , "BT 10 20 Td [(x) 120] TJ 0 0 Td (y) Tj ET"
      ] $ \input -> it ("preserves barriers: " ++ show input) $
        toList (optimizeProgram emptyWorkData (parse input)) `shouldBe` toList (parse input)

  describe "reference-model equivalence (property-based)" $ do
    it "matches an independent Tm/Tlm/TL evaluator on random positioning sequences" $
      property $ forAll genPositioningProgram $ \program ->
        let optimized = optimizeProgram emptyWorkData program
        in finalTextState optimized == finalTextState program
           && programComputedSize optimized <= programComputedSize program

    it "does not drift numerically over long sequences" $
      property $ forAll (genPositioningProgramOfLength 40) $ \program ->
        let optimized = optimizeProgram emptyWorkData program
        in finalTextState optimized == finalTextState program

-- | A minimal, independent reference implementation of the ISO 32000 Tm/Tlm/TL
-- semantics for BT, Tm, Td, TD, T* and TL. Deliberately not shared with
-- 'PDF.Graphics.Interpreter.OptimizeCommand.OptimizeTextMatrix' so that the
-- property tests do not merely check the implementation against itself.
finalTextState :: Program -> (TransformationMatrix, TransformationMatrix, Double)
finalTextState = foldl step (mempty, mempty, 0) . toList
 where
  step (tm, tlm, leading) command = case (cOperator command, nums command) of
    (GSBeginText, []) -> (mempty, mempty, leading)
    (GSEndText, []) -> (tm, tlm, leading)
    (GSSetTextMatrix, [a, b, c, d, e, f]) ->
      let m = TransformationMatrix a b c d e f in (m, m, leading)
    (GSMoveToNextLine, [x, y]) ->
      let m = tlm <> TransformationMatrix 1 0 0 1 x y in (m, m, leading)
    (GSMoveToNextLineLP, [x, y]) ->
      let m = tlm <> TransformationMatrix 1 0 0 1 x y in (m, m, -y)
    (GSNextLine, []) ->
      let m = tlm <> TransformationMatrix 1 0 0 1 0 (-leading) in (m, m, leading)
    (GSSetTextLeading, [l]) -> (tm, tlm, l)
    _anyOtherCommand -> (tm, tlm, leading)
  nums command = [n | GFXNumber n <- toList (cParameters command)]

genNumber :: Gen Double
genNumber = elements
  [0, 1, 2, 3, 4.5, -4.5, 6, -6, 10.25, -10.25, 0.125, -0.125, 100.5, -100.5]

genMatrixDiag :: Gen Double
genMatrixDiag = elements [1, 1.5, 2, -1, -2, 0.5, -0.5]

genMatrixOffDiag :: Gen Double
genMatrixOffDiag = elements [0, 0.5, -0.5, 1, -1]

genPositioningCommand :: Gen Command
genPositioningCommand = elements [1 :: Int, 2, 3, 4, 5] >>= \case
  1 -> do
    a <- genMatrixDiag
    d <- genMatrixDiag
    b <- genMatrixOffDiag
    c <- genMatrixOffDiag
    e <- genNumber
    f <- genNumber
    pure $ mkCommand GSSetTextMatrix (map GFXNumber [a, b, c, d, e, f])
  2 -> do
    x <- genNumber
    y <- genNumber
    pure $ mkCommand GSMoveToNextLine (map GFXNumber [x, y])
  3 -> do
    x <- genNumber
    y <- genNumber
    pure $ mkCommand GSMoveToNextLineLP (map GFXNumber [x, y])
  4 -> pure $ mkCommand GSNextLine []
  _ -> do
    l <- genNumber
    pure $ mkCommand GSSetTextLeading [GFXNumber l]

genPositioningProgramOfLength :: Int -> Gen Program
genPositioningProgramOfLength n = do
  body <- replicateM n genPositioningCommand
  pure $ mkProgram
    (mkCommand GSBeginText [] : body ++ [mkCommand GSEndText []])

genPositioningProgram :: Gen Program
genPositioningProgram = choose (0, 15) >>= genPositioningProgramOfLength
