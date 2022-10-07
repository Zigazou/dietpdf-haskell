{-# LANGUAGE OverloadedStrings #-}
module Pdf.Parser.DictionarySpec
  ( spec
  ) where

import           Test.Hspec                     ( describe
                                                , Spec
                                                )
import qualified Data.ByteString               as BS
import           Data.Map.Strict                ( fromList
                                                , empty
                                                )
import           Util.ParserHelper              ( itWith )
import           Pdf.Parser.Container           ( dictionaryP )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFNumber
                                                  , PDFDictionary
                                                  , PDFReference
                                                  , PDFArray
                                                  , PDFName
                                                  )
                                                )

dictionaryExamples :: [(BS.ByteString, PDFObject)]
dictionaryExamples =
  [ ("<</a 1>>"  , PDFDictionary (fromList [("a", PDFNumber 1.0)]))
  , ("<< /a 1 >>", PDFDictionary (fromList [("a", PDFNumber 1.0)]))
  , ("<<>>"      , PDFDictionary empty)
  , ("<<   >>"   , PDFDictionary empty)
  , ( "<</b 2/a 1>>"
    , PDFDictionary (fromList [("a", PDFNumber 1.0), ("b", PDFNumber 2.0)])
    )
  , ( "<</a<</b 2>>>>"
    , PDFDictionary
      (fromList [("a", PDFDictionary (fromList [("b", PDFNumber 2.0)]))])
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
    , PDFDictionary
      (fromList
        [ ("Type"    , PDFName "FontDescriptor")
        , ("FontName", PDFName "BAAAAA+LiberationSerif")
        , ("Flags"   , PDFNumber 4.0)
        , ( "FontBBox"
          , PDFArray
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
    )
  ]

spec :: Spec
spec = describe "dictionaryP"
  $ mapM_ (itWith "should work with " dictionaryP) dictionaryExamples
