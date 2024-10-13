module PDF.Graphics.Parser.DictionarySpec
  ( spec
  ) where

import Data.ByteString (ByteString)
import Data.PDF.GFXObject
    ( GFXObject (GFXName, GFXNumber, GFXReference)
    , mkEmptyGFXDictionary
    , mkGFXArray
    , mkGFXDictionary
    )

import PDF.Graphics.Parser.Container (dictionaryP)

import Test.Hspec (Spec, describe)

import Util.ParserHelper (itWith)

dictionaryExamples :: [(ByteString, GFXObject)]
dictionaryExamples =
  [ ("<</a 1>>"  , mkGFXDictionary [("a", GFXNumber 1.0)])
  , ("<< /a 1 >>", mkGFXDictionary [("a", GFXNumber 1.0)])
  , ("<<>>"      , mkEmptyGFXDictionary)
  , ("<<   >>"   , mkEmptyGFXDictionary)
  , ( "<</b 2/a 1>>"
    , mkGFXDictionary [("a", GFXNumber 1.0), ("b", GFXNumber 2.0)]
    )
  , ( "<</a<</b 2>>>>"
    , mkGFXDictionary [("a", mkGFXDictionary [("b", GFXNumber 2.0)])]
    )
  , ( "<</Type/FontDescriptor/FontName/BAAAAA+LiberationSerif\
      \/Flags 4\n\
      \/FontBBox[-543 -303 1277 981]/ItalicAngle 0\n\
      \/Ascent 0\n\
      \/Descent 0\n\
      \/CapHeight 981\n\
      \/StemV 80\n\
      \/FontFile2 7 0 R\n\
      \>>"
    , mkGFXDictionary
      [ ("Type"    , GFXName "FontDescriptor")
      , ("FontName", GFXName "BAAAAA+LiberationSerif")
      , ("Flags"   , GFXNumber 4.0)
      , ( "FontBBox"
        , mkGFXArray
          [ GFXNumber (-543.0)
          , GFXNumber (-303.0)
          , GFXNumber 1277.0
          , GFXNumber 981.0
          ]
        )
      , ("ItalicAngle", GFXNumber 0.0)
      , ("Ascent"     , GFXNumber 0.0)
      , ("Descent"    , GFXNumber 0.0)
      , ("CapHeight"  , GFXNumber 981.0)
      , ("StemV"      , GFXNumber 80.0)
      , ("FontFile2"  , GFXReference 7 0)
      ]
    )
  ]

spec :: Spec
spec = describe "dictionaryP"
  $ mapM_ (itWith "should work with " dictionaryP) dictionaryExamples
