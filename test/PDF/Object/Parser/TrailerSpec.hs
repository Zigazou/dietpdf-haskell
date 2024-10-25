module PDF.Object.Parser.TrailerSpec
  ( spec
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (empty, fromList)

import PDF.Object.Object (PDFObject (PDFDictionary, PDFReference, PDFTrailer))
import PDF.Object.Parser.Trailer (trailerP)

import Test.Hspec (Spec, describe)

import Util.ParserHelper (itWith)

trailerExamples :: [(ByteString, PDFObject)]
trailerExamples =
  [ ("trailer\n\
      \<<>>", PDFTrailer (PDFDictionary empty))
  , ( "trailer\r\n\
      \<</Root 18 0 R/Info 19 0 R>>"
    , PDFTrailer
      (PDFDictionary
        (fromList [("Root", PDFReference 18 0), ("Info", PDFReference 19 0)])
      )
    )
  ]

spec :: Spec
spec = describe "trailerP"
  $ mapM_ (itWith "should work with " trailerP) trailerExamples
