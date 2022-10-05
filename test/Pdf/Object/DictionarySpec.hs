{-# LANGUAGE OverloadedStrings #-}
module Pdf.Object.DictionarySpec
  ( spec
  ) where

import           Control.Monad                  ( forM_ )
import qualified Data.ByteString               as BS
import           Data.HashMap.Strict            ( fromList )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFArray
                                                  , PDFBool
                                                  , PDFDictionary
                                                  , PDFName
                                                  , PDFNumber
                                                  , PDFIndirectObject
                                                  , PDFTrailer
                                                  )
                                                , fromPDFObject
                                                , getValue
                                                , query
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

dictionaryExamples :: [(PDFObject, BS.ByteString)]
dictionaryExamples =
  [ (PDFDictionary (fromList []), "<<>>")
  , ( PDFDictionary
      (fromList
        [("AB", PDFNumber 1.0), ("CD", PDFName "AB"), ("EF", PDFBool True)]
      )
    , "<</EF true/AB 1/CD/AB>>"
    )
  , ( PDFDictionary
      (fromList
        [ ( "AB"
          , PDFArray [PDFNumber 1.0, PDFArray [PDFNumber 2.0], PDFNumber 3.0]
          )
        , ("CD", PDFName "AB")
        , ("EF", PDFBool True)
        ]
      )
    , "<</EF true/AB[1[2]3]/CD/AB>>"
    )
  ]

getValueExamples :: [(PDFObject, Maybe PDFObject)]
getValueExamples =
  [ (PDFDictionary (fromList []), Nothing)
  , (PDFNumber 3.0              , Nothing)
  , ( PDFDictionary
      (fromList
        [("AB", PDFNumber 1.0), ("Test", PDFName "AB"), ("EF", PDFBool True)]
      )
    , Just $ PDFName "AB"
    )
  , ( PDFDictionary
      (fromList
        [("AB", PDFNumber 1.0), ("CD", PDFName "AB"), ("EF", PDFBool True)]
      )
    , Nothing
    )
  , ( PDFIndirectObject
      10
      0
      (PDFDictionary
        (fromList
          [ ( "AB"
            , PDFArray [PDFNumber 1.0, PDFArray [PDFNumber 2.0], PDFNumber 3.0]
            )
          , ("CD", PDFName "AB")
          , ("EF", PDFBool True)
          ]
        )
      )
    , Nothing
    )
  , ( PDFIndirectObject
      10
      0
      (PDFDictionary
        (fromList
          [ ( "AB"
            , PDFArray [PDFNumber 1.0, PDFArray [PDFNumber 2.0], PDFNumber 3.0]
            )
          , ("Test", PDFNumber 4.0)
          , ("EF"  , PDFBool True)
          ]
        )
      )
    , Just $ PDFNumber 4.0
    )
  , ( PDFTrailer
      (PDFDictionary
        (fromList
          [ ( "AB"
            , PDFArray [PDFNumber 1.0, PDFArray [PDFNumber 2.0], PDFNumber 3.0]
            )
          , ("Test", PDFNumber 4.0)
          , ("EF"  , PDFBool True)
          ]
        )
      )
    , Just $ PDFNumber 4.0
    )
  ]

spec :: Spec
spec = describe "PDFDictionary" $ do
  forM_ dictionaryExamples $ \(example, expected) ->
    it ("should convert to bytestring " ++ show example)
      $          fromPDFObject example
      `shouldBe` expected

  forM_ getValueExamples $ \(example, expected) ->
    it ("should get value from Dictionary " ++ show example)
      $          query example (getValue "Test")
      `shouldBe` expected
