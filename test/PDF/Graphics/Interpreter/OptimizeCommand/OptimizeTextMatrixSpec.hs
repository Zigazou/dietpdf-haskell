module PDF.Graphics.Interpreter.OptimizeCommand.OptimizeTextMatrixSpec (spec) where

import Control.Monad (foldM, forM_)
import Control.Monad.State (State, execState)
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.PDF.Command (Command)
import Data.PDF.GraphicsState (gsTextState)
import Data.PDF.InterpreterState (InterpreterState, defaultInterpreterState, iGraphicsState, setTextMatrixS, saveStateS, restoreStateS)
import Data.PDF.Program (Program, parseProgram)
import Data.PDF.TextState (TextState (tsMatrix, tsLineMatrix, tsLeading, tsCharacterSpacing, tsWordSpacing))
import Data.PDF.TransformationMatrix (TransformationMatrix (TransformationMatrix))
import Data.PDF.WorkData (emptyWorkData)
import PDF.Graphics.Interpreter.OptimizeCommand.OptimizeTextMatrix (optimizeTextMatrix)
import PDF.Graphics.Interpreter.OptimizeProgram (optimizeProgram)
import PDF.Graphics.Parser.Stream (gfxParse)
import Test.Hspec (Spec, describe, it, shouldBe)

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
