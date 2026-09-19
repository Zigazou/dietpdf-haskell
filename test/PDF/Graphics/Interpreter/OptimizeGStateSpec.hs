module PDF.Graphics.Interpreter.OptimizeGStateSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.PDF.Command (mkCommand)
import Data.PDF.GFXObject
  ( GFXObject (GFXArray, GFXName, GFXNumber)
  , GSOperator (GSLineTo, GSMoveTo, GSSetLineWidth, GSSetMiterLimit, GSSetParameters, GSSetTextFont, GSSetLineDashPattern)
  )
import Data.PDF.PDFObject (PDFObject (PDFArray, PDFDictionary, PDFNumber))
import Data.PDF.PDFWork (evalPDFWorkT, getAdditionalGStates)
import Data.Sequence qualified as SQ
import Data.Map.Strict qualified as Map
import Data.PDF.Program (Program, mkProgram)

import PDF.Graphics.Interpreter.OptimizeGState (optimizeGState)

import Test.Hspec (Spec, describe, it, shouldBe)

import Util.Dictionary (Dictionary, mkDictionary)


optimizeGStateExamples :: [(Program, (Dictionary (Dictionary PDFObject), Program))]
optimizeGStateExamples =
  [ ( mempty, (mempty, mempty) )
  , ( mkProgram
        [ mkCommand GSMoveTo [GFXNumber 0, GFXNumber 0]
        , mkCommand GSLineTo [GFXNumber 10, GFXNumber 10]
        ]
    , ( mempty
      , mkProgram
          [ mkCommand GSMoveTo [GFXNumber 0, GFXNumber 0]
          , mkCommand GSLineTo [GFXNumber 10, GFXNumber 10]
          ]
      )
    )
  , ( mkProgram
        [ mkCommand GSSetLineWidth [GFXNumber 1]
        , mkCommand GSSetMiterLimit [GFXNumber 2]
        , mkCommand GSMoveTo [GFXNumber 0, GFXNumber 0]
        , mkCommand GSLineTo [GFXNumber 10, GFXNumber 10]
        ]
    , ( mkDictionary
          [ ( "0"
            , mkDictionary
                [ ( "LW", PDFNumber 1.0 )
                , ( "ML", PDFNumber 2.0 )
                ]
            )
          ]
      , mkProgram
        [ mkCommand GSSetParameters [GFXName "0"]
        , mkCommand GSMoveTo [GFXNumber 0, GFXNumber 0]
        , mkCommand GSLineTo [GFXNumber 10, GFXNumber 10]
        ]
      )
    )
  , ( mkProgram
        [ mkCommand GSMoveTo [GFXNumber 0, GFXNumber 0]
        , mkCommand GSSetLineWidth [GFXNumber 1]
        , mkCommand GSSetMiterLimit [GFXNumber 2]
        , mkCommand GSLineTo [GFXNumber 10, GFXNumber 10]
        ]
    , ( mkDictionary
          [ ( "0"
            , mkDictionary
                [ ( "LW", PDFNumber 1.0 )
                , ( "ML", PDFNumber 2.0 )
                ]
            )
          ]
      , mkProgram
        [ mkCommand GSMoveTo [GFXNumber 0, GFXNumber 0]
        , mkCommand GSSetParameters [GFXName "0"]
        , mkCommand GSLineTo [GFXNumber 10, GFXNumber 10]
        ]
      )
    )
  , ( mkProgram
        [ mkCommand GSSetLineWidth [GFXNumber 1]
        , mkCommand GSSetMiterLimit [GFXNumber 2]
        , mkCommand GSMoveTo [GFXNumber 0, GFXNumber 0]
        , mkCommand GSLineTo [GFXNumber 10, GFXNumber 10]
        , mkCommand GSSetLineWidth [GFXNumber 3]
        , mkCommand GSSetMiterLimit [GFXNumber 4]
        ]
    , ( mkDictionary
          [ ( "0"
            , mkDictionary
                [ ( "LW", PDFNumber 1.0 )
                , ( "ML", PDFNumber 2.0 )
                ]
            )
          , ( "1"
            , mkDictionary
                [ ( "LW", PDFNumber 3.0 )
                , ( "ML", PDFNumber 4.0 )
                ]
            )
          ]
      , mkProgram
        [ mkCommand GSSetParameters [GFXName "0"]
        , mkCommand GSMoveTo [GFXNumber 0, GFXNumber 0]
        , mkCommand GSLineTo [GFXNumber 10, GFXNumber 10]
        , mkCommand GSSetParameters [GFXName "1"]
        ]
      )
    )
  ]

spec :: Spec
spec = do
  describe "optimizeGState" $ do
    forM_ optimizeGStateExamples $ \(example, expected) ->
      it ("should optimize GState " ++ show example)
        $   evalPDFWorkT (optimizeGState example)
        >>= \case
          Right result -> result `shouldBe` snd expected
          Left _failed -> fail "Failed to optimize GState"

    it "preserves font selection between factorizable commands" $ do
      let font = mkCommand GSSetTextFont [GFXName "F1", GFXNumber 12]
          program = mkProgram [mkCommand GSSetLineWidth [GFXNumber 1], font,
                               mkCommand GSSetMiterLimit [GFXNumber 2]]
      result <- evalPDFWorkT (optimizeGState program)
      result `shouldBe` Right (mkProgram
        [mkCommand GSSetParameters [GFXName "0"], font,
         mkCommand GSSetParameters [GFXName "1"]])

    it "uses the last value when a parameter is set repeatedly" $ do
      result <- evalPDFWorkT $ do
        _ <- optimizeGState (mkProgram
          [mkCommand GSSetLineWidth [GFXNumber 1],
           mkCommand GSSetLineWidth [GFXNumber 3]])
        getAdditionalGStates
      result `shouldBe` Right (Map.singleton "0"
        (PDFDictionary (mkDictionary [("LW", PDFNumber 3)])))

    it "stores dash arrays without an extra array nesting level" $ do
      let patternValues = SQ.fromList [GFXNumber 3, GFXNumber 2]
      result <- evalPDFWorkT $ do
        _ <- optimizeGState (mkProgram
          [mkCommand GSSetLineDashPattern [GFXArray patternValues, GFXNumber 1]])
        getAdditionalGStates
      result `shouldBe` Right (Map.singleton "0" (PDFDictionary (mkDictionary
        [("D", PDFArray (SQ.fromList
          [PDFArray (SQ.fromList [PDFNumber 3, PDFNumber 2]), PDFNumber 1]))])))
