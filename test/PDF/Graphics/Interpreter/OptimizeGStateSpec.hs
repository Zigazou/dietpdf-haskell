module PDF.Graphics.Interpreter.OptimizeGStateSpec (spec) where

import Control.Monad (forM_)

import Data.ByteString (ByteString)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as Map
import Data.PDF.PDFObject (PDFObject (PDFDictionary, PDFNumber, PDFIndirectObject, PDFArray))
import Data.PDF.PDFWork (evalPDFWorkT, getAdditionalGStates)
import Data.PDF.Program (Program, parseProgram)
import Data.PDF.WorkData (WorkData (wAdditionalGStates, wNameTranslations, wPDF), emptyWorkData)
import Data.PDF.PDFPartition (PDFPartition (ppObjectsWithoutStream))
import Data.PDF.Resource (Resource (ResExtGState))
import Data.Sequence qualified as SQ

import PDF.Graphics.Interpreter.OptimizeGState (gStateCost, optimizeGState, planGState)
import PDF.Graphics.Parser.Stream (gfxParse)

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

parse :: ByteString -> Program
parse input = either (error . show) parseProgram (gfxParse input)

spec :: Spec
spec = describe "optimizeGState" $ do
  forM_ ["", "1 w", "1 w 2 M", "w", "[3 /Bad]0 d", "1 w /F1 12 Tf 2 M", "3 4 m 5 6 l"] $ \input ->
    it ("does not allocate unprofitable resources for " ++ show input) $ do
      let program = parse input
      result <- evalPDFWorkT $ do
        optimized <- optimizeGState program
        resources <- getAdditionalGStates
        return (optimized, resources)
      result `shouldBe` Right (program, mempty)

  it "amortizes one shared dictionary over repeated runs across painting barriers" $ do
    let run = parse "2 w 3 M 0 0 m 10 10 l S"
        program = mconcat (replicate 40 run)
        (optimized, work) = planGState emptyWorkData program
    Map.size (wAdditionalGStates work) `shouldBe` 1
    SQ.length optimized `shouldBe` 160
    gStateCost work optimized `shouldSatisfy` (< gStateCost emptyWorkData program)
    wAdditionalGStates work `shouldBe` Map.singleton "0"
      (PDFDictionary (Map.fromList [("LW", PDFNumber 2), ("ML", PDFNumber 3)]))
    let (again, reusedWork) = planGState work program
    again `shouldBe` optimized
    wAdditionalGStates reusedWork `shouldBe` wAdditionalGStates work
    wNameTranslations reusedWork `shouldBe` wNameTranslations work

  it "uses the last value of a repeated parameter" $ do
    let program = mconcat (replicate 40 (parse "1 w 3 w 0 0 m 10 10 l S"))
        (_, work) = planGState emptyWorkData program
    wAdditionalGStates work `shouldBe` Map.singleton "0"
      (PDFDictionary (Map.singleton "LW" (PDFNumber 3)))

  it "accounts for the encoded length of an existing resource name" $ do
    let name :: ByteString
        name = "AnExtremelyLongGraphicsStateResourceName"
        resource = ResExtGState name
        work = emptyWorkData
          { wAdditionalGStates = Map.singleton name
              (PDFDictionary (Map.singleton "LW" (PDFNumber 2)))
          , wNameTranslations = Map.singleton resource resource
          }
        program = parse "2 w"
        (optimized, updated) = planGState work program
    optimized `shouldBe` program
    wNameTranslations updated `shouldBe` wNameTranslations work

  it "does not allocate resources while evaluating rejected candidates" $ do
    let (_, work) = planGState emptyWorkData (parse "1 w 2 M")
    wAdditionalGStates work `shouldBe` mempty
    wNameTranslations work `shouldBe` mempty

  it "amortizes repeated dash runs without changing their array structure" $ do
    let program = mconcat (replicate 40 (parse "[3 2]1 d 0 0 m 10 10 l S"))
        (optimized, work) = planGState emptyWorkData program
    Map.size (wAdditionalGStates work) `shouldBe` 1
    SQ.length optimized `shouldBe` 160
    gStateCost work optimized `shouldSatisfy` (< gStateCost emptyWorkData program)
    wAdditionalGStates work `shouldBe` Map.singleton "0"
      (PDFDictionary (Map.singleton "D" (PDFArray (SQ.fromList
        [PDFArray (SQ.fromList [PDFNumber 3, PDFNumber 2]), PDFNumber 1]))))

  it "keeps font selections between profitable state runs" $ do
    let program = mconcat (replicate 40 (parse "2 w 3 M /F1 12 Tf 4 w 5 M /F2 10 Tf"))
        (optimized, work) = planGState emptyWorkData program
    Map.size (wAdditionalGStates work) `shouldBe` 2
    optimized `shouldBe` mconcat (replicate 40
      (parse "/0 gs /F1 12 Tf /1 gs /F2 10 Tf"))

  it "charges for resource duplication across pages" $ do
    let program = mconcat (replicate 40 (parse "2 w 3 M 0 0 m 10 10 l S"))
        document = mempty { ppObjectsWithoutStream = IM.fromList
          [(index, PDFIndirectObject index 0
              (PDFDictionary (Map.singleton "Resources" (PDFDictionary mempty))))
           | index <- [1..100]] }
        (optimized, work) = planGState (emptyWorkData { wPDF = document }) program
    optimized `shouldBe` program
    wAdditionalGStates work `shouldBe` mempty
