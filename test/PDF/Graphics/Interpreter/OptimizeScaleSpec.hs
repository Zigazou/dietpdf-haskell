module PDF.Graphics.Interpreter.OptimizeScaleSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.Map.Strict qualified as Map
import Data.IntMap.Strict qualified as IM
import Data.PDF.PDFObject (PDFObject (PDFDictionary, PDFIndirectObject, PDFName, PDFNull, PDFNumber, PDFReference), mkPDFArray)
import Data.PDF.Command (mkCommand)
import Data.PDF.GFXObject
  ( GFXObject (GFXName, GFXNumber)
  , GSOperator (GSLineTo, GSMoveTo, GSPaintShapeColourShading, GSPaintXObject, GSRectangle, GSRestoreGS, GSSaveGS, GSSetCTM, GSSetLineWidth)
  )
import Data.PDF.Program (Program, mkProgram, parseProgram)

import PDF.Graphics.Interpreter.OptimizeScale (buildScaleResources, isScaleOptimizable, optimizeScale)
import PDF.Graphics.Parser.Stream (gfxParse)

import Util.Dictionary (Dictionary)

import Test.Hspec (Spec, describe, it, shouldBe)

isScaleOptimizableExamples :: [(String, Program, Bool)]
isScaleOptimizableExamples =
  [ ( "True for an empty program"
    , mkProgram []
    , True
    )
  , ( "True for a program with only path construction commands"
    , mkProgram
        [ mkCommand GSMoveTo [GFXNumber 10, GFXNumber 20]
        , mkCommand GSLineTo [GFXNumber 30, GFXNumber 40]
        , mkCommand GSRectangle [GFXNumber 0, GFXNumber 0, GFXNumber 100, GFXNumber 100]
        ]
    , True
    )
  , ( "True for a program with graphics state commands"
    , mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSSetLineWidth [GFXNumber 2.5]
        , mkCommand GSSetCTM [GFXNumber 1, GFXNumber 0, GFXNumber 0, GFXNumber 1, GFXNumber 10, GFXNumber 20]
        , mkCommand GSRestoreGS []
        ]
    , True
    )
  ,( "False for a program containing GSPaintXObject"
    , mkProgram
        [ mkCommand GSMoveTo [GFXNumber 10, GFXNumber 20]
        , mkCommand GSPaintXObject [GFXName "Image1"]
        ]
    , False
    )
  , ( "False for a program with only GSPaintXObject"
    , mkProgram
        [ mkCommand GSPaintXObject [GFXName "Form1"]
        ]
    , False
    )

  , ( "False for a program containing GSPaintShapeColourShading"
    , mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSPaintShapeColourShading [GFXName "Sh1"]
        , mkCommand GSRestoreGS []
        ]
    , False
    )
  , ( "False for a program with only GSPaintShapeColourShading"
    , mkProgram
        [ mkCommand GSPaintShapeColourShading [GFXName "Shading1"]
        ]
    , False
    )
  , ( "False for a program containing both paint operators"
    , mkProgram
            [ mkCommand GSMoveTo [GFXNumber 0, GFXNumber 0]
            , mkCommand GSPaintXObject [GFXName "Image1"]
            , mkCommand GSLineTo [GFXNumber 100, GFXNumber 100]
            , mkCommand GSPaintShapeColourShading [GFXName "Sh1"]
            ]
    , False
    )
  , ( "False when GSPaintXObject appears at the end"
    , mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSSetCTM [GFXNumber 1, GFXNumber 0, GFXNumber 0, GFXNumber 1, GFXNumber 0, GFXNumber 0]
        , mkCommand GSMoveTo [GFXNumber 10, GFXNumber 20]
        , mkCommand GSLineTo [GFXNumber 30, GFXNumber 40]
        , mkCommand GSRestoreGS []
        , mkCommand GSPaintXObject [GFXName "Logo"]
        ]
    , False
    )
  , ( "False when GSPaintShapeColourShading appears at the beginning"
    , mkProgram
        [ mkCommand GSPaintShapeColourShading [GFXName "Background"]
        , mkCommand GSMoveTo [GFXNumber 10, GFXNumber 20]
        , mkCommand GSLineTo [GFXNumber 30, GFXNumber 40]
        ]
    , False
    )
  , ( "True for a complex program without paint operators"
    , mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSSetLineWidth [GFXNumber 1.0]
        , mkCommand GSMoveTo [GFXNumber 10, GFXNumber 20]
        , mkCommand GSLineTo [GFXNumber 100, GFXNumber 20]
        , mkCommand GSLineTo [GFXNumber 100, GFXNumber 100]
        , mkCommand GSLineTo [GFXNumber 10, GFXNumber 100]
        , mkCommand GSRectangle [GFXNumber 50, GFXNumber 50, GFXNumber 30, GFXNumber 30]
        , mkCommand GSSetCTM [GFXNumber 1, GFXNumber 0, GFXNumber 0, GFXNumber 1, GFXNumber 5, GFXNumber 5]
        , mkCommand GSRestoreGS []
        ]
    , True
    )
  ]

spec :: Spec
spec = do
  describe "ExtGState scale compatibility" $ do
    let parse input = either (error . show) parseProgram (gfxParse input)
        resources :: PDFObject -> Maybe (Dictionary PDFObject)
        resources state = Just (Map.singleton "ExtGState"
          (PDFDictionary (Map.singleton "GS" state)))
        classify = buildScaleResources mempty . resources . PDFDictionary . Map.fromList
        program = parse "/GS gs 2 w .1 .2 m .3 .4 l S"
    it "scales paths with opacity and blend mode resources" $ do
      let states = classify [("ca", PDFNumber 0.5), ("BM", PDFName "Multiply")]
      isScaleOptimizable states program `shouldBe` True
      optimizeScale states 10 program `shouldBe`
        parse "q .1 0 0 .1 0 0 cm /GS gs 20 w 1 2 m 3 4 l S Q"
    forM_ [("Font", mkPDFArray [PDFReference 9 0, PDFNumber 12])
          ,("LW", PDFNumber 2)
          ,("D", mkPDFArray [mkPDFArray [PDFNumber 3, PDFNumber 2], PDFNumber 1])
          ,("SMask", PDFDictionary (Map.singleton "S" (PDFName "Alpha")))
          ,("Unknown", PDFNull)] $ \(key, entry) ->
      it ("rejects scale-sensitive or unsupported entry " ++ show key) $ do
        let states = classify [(key, entry)]
        isScaleOptimizable states program `shouldBe` False
        optimizeScale states 10 program `shouldBe` program
    it "accepts a disabled soft mask" $
      isScaleOptimizable (classify [("SMask", PDFName "None")]) program `shouldBe` True
    it "ignores unsafe states that the program does not use" $
      isScaleOptimizable (Map.fromList [("GS", True), ("Unused", False)]) program
        `shouldBe` True
    it "rejects malformed gs operands" $
      forM_ ["gs", "1 gs", "/GS /GS gs"] $ \input ->
        isScaleOptimizable (Map.singleton "GS" True) (parse input) `shouldBe` False
    it "resolves indirect categories and state dictionaries" $ do
      let objects = IM.fromList
            [(1, PDFIndirectObject 1 0 (PDFDictionary (Map.singleton "GS" (PDFReference 2 0))))
            ,(2, PDFIndirectObject 2 0 (PDFDictionary (Map.singleton "CA" (PDFNumber 0.5))))]
          states = buildScaleResources objects
            (Just (Map.singleton "ExtGState" (PDFReference 1 0)))
      isScaleOptimizable states program `shouldBe` True
    it "rejects unresolved, cyclic, and malformed resources" $
      forM_ [PDFReference 1 0, PDFReference 2 0, PDFNumber 3] $ \entry -> do
        let objects = IM.singleton 1 (PDFIndirectObject 1 0 (PDFReference 1 0))
        isScaleOptimizable (buildScaleResources objects (resources entry)) program
          `shouldBe` False
    it "keeps identical names in different resource scopes independent" $ do
      isScaleOptimizable (classify [("ca", PDFNumber 0.5)]) program `shouldBe` True
      isScaleOptimizable (classify [("LW", PDFNumber 1)]) program `shouldBe` False

  describe "text state scaling" $ do
    it "does not rescale state supplied by an ExtGState dictionary" $ do
      let program = either (error . show) parseProgram $ gfxParse "BT /GS gs (A) Tj ET"
      isScaleOptimizable mempty program `shouldBe` False
      optimizeScale mempty 100 program `shouldBe` program
    it "rescales quote spacing and text rise along with font size" $ do
      let parse input = either (error . show) parseProgram (gfxParse input)
      optimizeScale mempty 10 (parse "BT /F 10 Tf 2 Ts 3 4 (A) \" ET")
        `shouldBe` parse "q .1 0 0 .1 0 0 cm BT /F 100 Tf 20 Ts 30 40 (A) \" ET Q"
  describe "inline image scaling" $
    it "preserves image size and placement at every candidate scale" $ do
      let program = either (error . show) parseProgram $ gfxParse
            "q 7 0 0 61 7.1 .1 cm BI /W 1 /H 1 /BPC 1 /IM true ID \x80 EI Q"
      isScaleOptimizable mempty program `shouldBe` False
      forM_ [1, 10, 100, 1000] $ \scale ->
        optimizeScale mempty scale program `shouldBe` program

  describe "isScaleOptimizable"
    $ forM_ isScaleOptimizableExamples
    $ \(message, example, expected) -> do
        it ("should return " ++ message)
          $ isScaleOptimizable mempty example `shouldBe` expected
