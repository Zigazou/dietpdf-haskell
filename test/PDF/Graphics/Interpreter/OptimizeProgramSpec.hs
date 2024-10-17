module PDF.Graphics.Interpreter.OptimizeProgramSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.ByteString qualified as BS
import Data.PDF.Command (Command, mkCommand)
import Data.PDF.GFXObject
    ( GFXObject (GFXName, GFXNumber, GFXString)
    , GSOperator (GSBeginText, GSMoveTo, GSRestoreGS, GSSaveGS, GSSetCTM, GSSetTextFont, GSSetTextMatrix, GSShowManyText, GSUnknown)
    , mkGFXArray
    )
import Data.PDF.Program (Program, mkProgram, parseProgram)

import PDF.Graphics.Interpreter.OptimizeProgram
    ( findRelatedSave
    , optimizeProgram
    )
import PDF.Graphics.Parser.Stream (gfxParse)

import Test.Hspec (Spec, describe, it, shouldBe)

optimizeProgramExamples :: [(BS.ByteString, Program)]
optimizeProgramExamples =
  [ ( "", mkProgram [] )
  , ( "1.000042 2.421 m"
    ,  mkProgram [ mkCommand GSMoveTo [GFXNumber 1.0, GFXNumber 2.42]]
    )
  , ( "q cm Q"
    , mkProgram
        [ mkCommand GSSaveGS []
        , mkCommand GSSetCTM []
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
        , mkCommand GSSetTextFont [GFXName "R7", GFXNumber 11.04]
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
                [ GFXString "A", GFXNumber (-4.33874)
                , GFXString "B", GFXNumber 6.53732
                ]
            ]
        ]
    )
  ]

pSave, pRestore, pDummy :: Command
pSave    = mkCommand GSSaveGS []
pRestore = mkCommand GSRestoreGS []
pDummy   = mkCommand (GSUnknown "dummy") []

findRelatedSaveExamples :: [(Program, (Program, Program))]
findRelatedSaveExamples =
  [ ( mkProgram [ pSave, pDummy ]
    , (mempty, mkProgram [ pSave, pDummy ])
    )
  , ( mkProgram [ pSave, pSave ]
    , ( mkProgram [pSave], mkProgram [pSave] )
    )
  , ( mkProgram [ pSave, pSave, pDummy, pRestore ]
    , ( mempty
      , mkProgram [ pSave, pSave, pDummy, pRestore ]
      )
    )
  ]

spec :: Spec
spec = do
  describe "optimizeProgram" $
    forM_ optimizeProgramExamples $ \(example, expected) -> do
      it ("should work with " ++ show example)
        $          optimizeProgram . parseProgram <$> gfxParse example
        `shouldBe` Right expected

  describe "findRelatedSave" $
    forM_ findRelatedSaveExamples $ \(example, expected) -> do
      it ("should work with " ++ show example)
        $          findRelatedSave example
        `shouldBe` Just expected
