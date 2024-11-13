module PDF.Document.ResourcesSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.PDF.PDFDocument (PDFDocument, fromList)
import Data.PDF.PDFObject
  ( PDFObject (PDFEndOfFile, PDFIndirectObject, PDFNumber, PDFReference, PDFVersion)
  , mkPDFDictionary
  )
import Data.PDF.PDFWork (evalPDFWorkT)
import Data.PDF.Resource (Resource (ResFont))
import Data.Set (Set)
import Data.Set qualified as Set

import PDF.Document.Resources (getAllResourceNames)
import PDF.Processing.PDFWork (importObjects)

import Test.Hspec (Spec, describe, it, shouldBe)


getAllResourceNamesExamples :: [(Int, PDFDocument, Set Resource)]
getAllResourceNamesExamples =
  [ ( 0
    , fromList
        [ PDFVersion "1.4"
        , PDFIndirectObject 1 0 (mkPDFDictionary [("ID", PDFNumber 3)])
        , PDFEndOfFile
        , PDFIndirectObject 2 0 (mkPDFDictionary [("ID", PDFNumber 4)])
        ]
    , mempty
    )
  , ( 1
    , fromList
        [ PDFVersion "1.4"
        , PDFIndirectObject 1 0
            ( mkPDFDictionary
              [ ( "Resources"
                , mkPDFDictionary
                    [ ("Font"
                      , mkPDFDictionary [("a", PDFEndOfFile)]
                      )
                    ]
                )
              ]
            )
        ]
    , Set.fromList [ ResFont "a" ]
    )
  , ( 2
    , fromList
        [ PDFVersion "1.4"
        , PDFIndirectObject 1 0
            ( mkPDFDictionary [( "Resources", PDFReference 2 0 )] )
        , PDFIndirectObject 2 0
            ( mkPDFDictionary
                [ ( "Font"
                  , mkPDFDictionary [("a", PDFEndOfFile)]
                  )
                ]
            )
        ]
    , Set.fromList [ResFont "a"]
    )
  ]

spec :: Spec
spec = do
  describe "getAllResourceNames"
    $ forM_ getAllResourceNamesExamples
    $ \(identifier, example, expected) ->
        it ("should find all resource names for example " ++ show identifier) $ do
          optimized <- evalPDFWorkT (importObjects example >> getAllResourceNames)
          optimized `shouldBe` Right expected
