module PDF.Graphics.Interpreter.OptimizeProgramSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.ByteString (ByteString)
import Data.PDF.Command (mkCommand)
import Data.PDF.GFXObject
  ( GFXObject (GFXName, GFXNumber, GFXString)
  , GSOperator (GSBeginText, GSEndPath, GSMoveTo, GSRestoreGS, GSSaveGS, GSSetCTM, GSSetTextFont, GSSetTextMatrix, GSShowManyText)
  , mkGFXArray
  )
import Data.PDF.Program (Program, mkProgram, parseProgram)
import Data.PDF.WorkData (emptyWorkData)

import PDF.Graphics.Interpreter.OptimizeProgram (optimizeProgram)
import PDF.Graphics.Parser.Stream (gfxParse)

import Test.Hspec (Spec, describe, it, shouldBe)

optimizeProgramExamples :: [(ByteString, Program)]
optimizeProgramExamples =
  [ ( "", mkProgram [] )
  , ( "1.000042 2.421 m n"
    ,  mkProgram
        [ mkCommand GSMoveTo [GFXNumber 1.0, GFXNumber 2.42]
        , mkCommand GSEndPath []
        ]
    )
  , ( "q cm n Q"
    , mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSSetCTM []
        , mkCommand GSEndPath []
        , mkCommand GSRestoreGS []
        ]
    )
  , ( "q 0.12 0 0 0.12 0 0 cm\n\
      \q\n\
      \8.33333 0 0 8.33333 0 0 cm BT\n\
      \/R7 11.04 Tf\n\
      \0.999402 0 0 1 471.24 38.6002 Tm\n\
      \[(A)-4.33874(B)6.53732]TJ"
    , mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSSetCTM
            [ GFXNumber 0.12
            , GFXNumber 0.0
            , GFXNumber 0.0
            , GFXNumber 0.12
            , GFXNumber 0.0
            , GFXNumber 0.0
            ]
        , mkCommand GSSaveGS []
        , mkCommand GSSetCTM
            [ GFXNumber 8.33333
            , GFXNumber 0.0
            , GFXNumber 0.0
            , GFXNumber 8.33333
            , GFXNumber 0.0
            , GFXNumber 0.0
            ]
        , mkCommand GSBeginText []
        , mkCommand GSSetTextMatrix
            [ GFXNumber 0.9994
            , GFXNumber 0.0
            , GFXNumber 0.0
            , GFXNumber 1.0
            , GFXNumber 471.24
            , GFXNumber 38.6002
            ]
        , mkCommand GSSetTextFont [GFXName "R7", GFXNumber 11.04]
        , mkCommand GSShowManyText
            [ mkGFXArray
                [ GFXString "A", GFXNumber (-4.3387)
                , GFXString "B", GFXNumber 6.5373
                ]
            ]
        ]
    )
  ]

spec :: Spec
spec = do
  describe "optimizeProgram" $
    forM_ optimizeProgramExamples $ \(example, expected) -> do
      it ("should work with " ++ show example)
        $          optimizeProgram emptyWorkData . parseProgram <$> gfxParse example
        `shouldBe` Right expected

  describe "graphics state regressions" $ do
    let parse input = either (error . show) parseProgram (gfxParse input)
        optimize = optimizeProgram emptyWorkData . parse
    forM_
      [ "/DeviceRGB CS 1 0 0 SC 0 0 m 10 10 l S"
      , "/DeviceRGB cs 1 0 0 sc 0 0 10 10 re f"
      , "1 0 0 RG 0 1 0 SC 0 0 m 10 10 l S"
      , "1 0 0 rg 0 1 0 sc 0 0 10 10 re f"
      , "0.5 0.5 0.5 RG 1 0 0 SC 0 0 m 10 10 l S"
      , "/First gs /Second gs 0 0 m 10 10 l S"
      , "/External gs 1 w 0 0 m 10 10 l S"
      , "/External gs [] 0 d 0 0 m 10 10 l S"
      , "/External gs /RelativeColorimetric ri 0 0 m 10 10 l S"
      , "/CS1 CS 0.5 SC 0 0 m 10 10 l S /CS2 CS 0.5 SC 1 1 m 20 20 l S"
      , "/CS1 cs 0.5 sc 0 0 10 10 re f /CS1 cs 0.5 sc 1 1 20 20 re f"
      ] $ \input ->
        it ("preserves state dependencies in " ++ show input) $
          optimize input `shouldBe` parse input

    it "removes repeated dash settings across painting commands" $
      optimize "[3 2]1 d 0 0 m 10 10 l S [3 2]1 d 1 1 m 20 20 l S"
        `shouldBe` parse "[3 2]1 d 0 0 m 10 10 l S 1 1 m 20 20 l S"

    it "restores dash knowledge with q/Q" $
      optimize "[3 2]1 d q [4 2]0 d 0 0 m 10 10 l S Q [3 2]1 d 1 1 m 20 20 l S"
        `shouldBe` parse "[3 2]1 d q [4 2]0 d 0 0 m 10 10 l S Q 1 1 m 20 20 l S"

    it "preserves tiny nonzero dash lengths" $
      optimize "[0.0001 0.0002]0 d 0 0 m 10 10 l S"
        `shouldBe` parse "[0.0001 0.0002]0 d 0 0 m 10 10 l S"

    it "eliminates overwritten independent state settings before painting" $
      optimize "2 w 1 J 3 w 0 0 m 10 10 l S"
        `shouldBe` parse "3 w 1 J 0 0 m 10 10 l S"
