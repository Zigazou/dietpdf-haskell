{-# LANGUAGE OverloadedStrings #-}
module Pdf.Object.Parser.DictionarySpec
  ( spec
  ) where

import           Test.Hspec                     ( describe
                                                , Spec
                                                )
import qualified Data.ByteString               as BS
import           Util.ParserHelper              ( itWith )
import           Pdf.Object.Parser.Container    ( dictionaryP )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFNumber
                                                  , PDFReference
                                                  , PDFName
                                                  , PDFHexString
                                                  , PDFString
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
  , ( "<<\n\
      \/Type /Annot\n\
      \/F 4\n\
      \/Subtype /Link\n\
      \/Border [0 0 0]\n\
      \/Rect [264.976 234.538 516.905 254.607]\n\
      \/Contents <FEFF00680074007400700073003A002F002F006700690074006800750062002E0063006F006D002F007A006900670061007A006F0075>\n\
      \/A <<\n\
      \/Type /Action\n\
      \/S /URI\n\
      \/URI (https://github.com/zigazou)\n\
      \>>\n\
      \>>"
    , mkPDFDictionary
      [ ("Type"   , PDFName "Annot")
      , ("F"      , PDFNumber 4)
      , ("Subtype", PDFName "Link")
      , ("Border", mkPDFArray [PDFNumber 0, PDFNumber 0, PDFNumber 0])
      , ( "Rect"
        , mkPDFArray
          [ PDFNumber 264.976
          , PDFNumber 234.538
          , PDFNumber 516.905
          , PDFNumber 254.607
          ]
        )
      , ( "Contents"
        , PDFHexString
          "feff00680074007400700073003a002f002f006700690074006800750062002e0063006f006d002f007a006900670061007a006f0075"
        )
      , ( "A"
        , mkPDFDictionary
          [ ("Type", PDFName "Action")
          , ("S"   , PDFName "URI")
          , ("URI" , PDFString "https://github.com/zigazou")
          ]
        )
      ]
    )
  ]

spec :: Spec
spec = describe "dictionaryP"
  $ mapM_ (itWith "should work with " dictionaryP) dictionaryExamples
