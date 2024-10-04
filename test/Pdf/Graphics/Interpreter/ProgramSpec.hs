module Pdf.Graphics.Interpreter.ProgramSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.ByteString qualified as BS
import Data.Sequence qualified as SQ

import Pdf.Graphics.Interpreter.Command (Command (Command), mkCommand)
import Pdf.Graphics.Interpreter.Program (Program, optimizeProgram, parseProgram)
import Pdf.Graphics.Object
    ( GFXObject (GFXName, GFXNumber, GFXOperator, GFXString)
    , GSOperator (GSBeginText, GSMoveTo, GSRestoreGS, GSSaveGS, GSSetCTM, GSSetTextFont, GSSetTextMatrix, GSShowManyText)
    , mkGFXArray
    )
import Pdf.Graphics.Objects (Objects)
import Pdf.Graphics.Parser.Stream (gfxParse)

import Test.Hspec (Spec, describe, it, shouldBe)

parseProgramExamples :: [(Objects, Program)]
parseProgramExamples =
  [ ( SQ.fromList []
    , SQ.fromList []
    )
  , ( SQ.fromList
        [ GFXNumber 1.0
        , GFXNumber 2.0
        , GFXNumber 3.0
        , GFXOperator GSSetCTM
        ]
    , SQ.fromList
        [ Command GSSetCTM
                  (SQ.fromList [GFXNumber 1.0,GFXNumber 2.0,GFXNumber 3.0])
        ]
    )
  , ( SQ.fromList
        [ GFXOperator GSSaveGS
        , GFXOperator GSSetCTM
        , GFXOperator GSRestoreGS
        ]
    , SQ.fromList
        [ Command GSSaveGS SQ.empty
        , Command GSSetCTM SQ.empty
        , Command GSRestoreGS SQ.empty
        ]
    )
  ]

optimizeProgramExamples :: [(BS.ByteString, Program)]
optimizeProgramExamples =
  [ ( ""
    , SQ.fromList []
    )
  , ( "1.000042 2.421 m"
    ,  SQ.fromList
        [ Command GSMoveTo
                  ( SQ.fromList
                    [ GFXNumber 1.0
                    , GFXNumber 2.4
                    ]
                  )
        ]
    )
  , ( "q cm Q"
    , SQ.fromList
        [ Command GSSaveGS SQ.empty
        , Command GSSetCTM SQ.empty
        , Command GSRestoreGS SQ.empty
        ]
    )
  , ( "q 0.12 0 0 0.12 0 0 cm\n\
      \q\n\
      \8.33333 0 0 8.33333 0 0 cm BT\n\
      \/R7 11.04 Tf\n\
      \0.999402 0 0 1 471.24 38.6002 Tm\n\
      \[(A)-4.33874(B)6.53732]TJ"
    , SQ.fromList
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
        , mkCommand GSSetTextFont [GFXName "R7", GFXNumber 11.0]
        , mkCommand GSSetTextMatrix
            [ GFXNumber 0.999402
            , GFXNumber 0.0
            , GFXNumber 0.0
            , GFXNumber 1.0
            , GFXNumber 471.24
            , GFXNumber 38.6002
            ]
        , mkCommand GSShowManyText
            [ mkGFXArray
                [ GFXString "A", GFXNumber (-4.34)
                , GFXString "B", GFXNumber 6.54
                ]
            ]
        ]
    )
  ]

spec :: Spec
spec = do
  describe "parseProgram" $
    forM_ parseProgramExamples $ \(example, expected) -> do
      it ("should work with " ++ show example)
        $          parseProgram example
        `shouldBe` expected

  describe "optimizeProgram" $
    forM_ optimizeProgramExamples $ \(example, expected) -> do
      it ("should work with " ++ show example)
        $          optimizeProgram . parseProgram <$> gfxParse example
        `shouldBe` Right expected
