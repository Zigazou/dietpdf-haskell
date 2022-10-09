{-# LANGUAGE OverloadedStrings #-}
module Pdf.Parser.DictionarySpec
  ( spec
  ) where

import           Test.Hspec                     ( describe
                                                , Spec
                                                )
import qualified Data.ByteString               as BS
import           Util.ParserHelper              ( itWith )
import           Pdf.Parser.Container           ( dictionaryP )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFNumber
                                                  , PDFReference
                                                  , PDFName
                                                  )
                                                , mkPDFArray
                                                , mkPDFDictionary
                                                , mkEmptyPDFDictionary
                                                )

dictionaryExamples :: [(BS.ByteString, PDFObject)]
dictionaryExamples =
  [ ("<</a 1>>"  , mkPDFDictionary [("a", PDFNumber 1.0)])
  , ("<< /a 1 >>", mkPDFDictionary [("a", PDFNumber 1.0)])
  , ("<<>>"      , mkEmptyPDFDictionary)
  , ("<<   >>"   , mkEmptyPDFDictionary)
  , ( "<</b 2/a 1>>"
    , mkPDFDictionary [("a", PDFNumber 1.0), ("b", PDFNumber 2.0)]
    )
  , ( "<</a<</b 2>>>>"
    , mkPDFDictionary [("a", mkPDFDictionary [("b", PDFNumber 2.0)])]
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
    , mkPDFDictionary
      [ ("Type"    , PDFName "FontDescriptor")
      , ("FontName", PDFName "BAAAAA+LiberationSerif")
      , ("Flags"   , PDFNumber 4.0)
      , ( "FontBBox"
        , mkPDFArray
          [ PDFNumber (-543.0)
          , PDFNumber (-303.0)
          , PDFNumber 1277.0
          , PDFNumber 981.0
          ]
        )
      , ("ItalicAngle", PDFNumber 0.0)
      , ("Ascent"     , PDFNumber 0.0)
      , ("Descent"    , PDFNumber 0.0)
      , ("CapHeight"  , PDFNumber 981.0)
      , ("StemV"      , PDFNumber 80.0)
      , ("FontFile2"  , PDFReference 7 0)
      ]
    )
  ]

spec :: Spec
spec = describe "dictionaryP"
  $ mapM_ (itWith "should work with " dictionaryP) dictionaryExamples
