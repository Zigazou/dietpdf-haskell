module PDF.Graphics.Parser.ArraySpec
  ( spec
  ) where

import Data.ByteString (ByteString)
import Data.PDF.GFXObject
    ( GFXObject (GFXName, GFXNumber, GFXReference, GFXString)
    , mkGFXArray
    )

import PDF.Graphics.Parser.Container (arrayP)

import Test.Hspec (Spec, describe)

import Util.ParserHelper (itWith)

arrayExamples :: [(ByteString, GFXObject)]
arrayExamples =
  [ ("[1   2   3]", mkGFXArray [GFXNumber 1.0, GFXNumber 2.0, GFXNumber 3.0])
  , ("[ 1 2 3 ]"  , mkGFXArray [GFXNumber 1.0, GFXNumber 2.0, GFXNumber 3.0])
  , ( "[/ab/cd(abc)1 2 0 R]"
    , mkGFXArray
      [ GFXName "ab"
      , GFXName "cd"
      , GFXString "abc"
      , GFXNumber 1.0
      , GFXReference 2 0
      ]
    )
  , ( "[[1 2][3 4]]"
    , mkGFXArray
      [ mkGFXArray [GFXNumber 1.0, GFXNumber 2.0]
      , mkGFXArray [GFXNumber 3.0, GFXNumber 4.0]
      ]
    )
  ]

spec :: Spec
spec =
  describe "arrayP" $ mapM_ (itWith "should work with " arrayP) arrayExamples
