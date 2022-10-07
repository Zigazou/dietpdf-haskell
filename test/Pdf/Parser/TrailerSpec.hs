{-# LANGUAGE OverloadedStrings #-}
module Pdf.Parser.TrailerSpec
  ( spec
  ) where

import           Test.Hspec                     ( describe
                                                , Spec
                                                )

import qualified Data.ByteString               as BS
import           Util.ParserHelper              ( itWith )
import           Pdf.Parser.Trailer             ( trailerP )
import           Data.Map.Strict                ( fromList
                                                , empty
                                                )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFTrailer
                                                  , PDFDictionary
                                                  , PDFReference
                                                  )
                                                )

trailerExamples :: [(BS.ByteString, PDFObject)]
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
