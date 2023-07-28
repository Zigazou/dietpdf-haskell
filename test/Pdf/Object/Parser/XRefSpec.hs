module Pdf.Object.Parser.XRefSpec
  ( spec
  ) where

import           Test.Hspec                     ( describe
                                                , Spec
                                                )
import qualified Data.ByteString               as BS
import           Util.ParserHelper              ( itWith )
import           Pdf.Object.Parser.XRef         ( xrefP )
import           Pdf.Object.Object              ( PDFObject(PDFXRef)
                                                , XRefState
                                                  ( InUseEntry
                                                  , FreeEntry
                                                  )
                                                , XRefEntry(XRefEntry)
                                                , XRefSubsection(XRefSubsection)
                                                )


xrefExamples :: [(BS.ByteString, PDFObject)]
xrefExamples =
  [ ( "xref\n\
      \0 1\n\
      \0000000001 00002 n \n"
    , PDFXRef [XRefSubsection 0 1 [XRefEntry 1 2 InUseEntry]]
    )
  , ( "xref\n\
      \0 2\n\
      \0000000001 00002 n \n\
      \0000000003 00004 f\r\n\
      \4 1\n\
      \0000000005 00006 n \r"
    , PDFXRef
      [ XRefSubsection 0 2 [XRefEntry 1 2 InUseEntry, XRefEntry 3 4 FreeEntry]
      , XRefSubsection 4 1 [XRefEntry 5 6 InUseEntry]
      ]
    )
  ]

spec :: Spec
spec = describe "xrefP" $ mapM_ (itWith "should work with " xrefP) xrefExamples
