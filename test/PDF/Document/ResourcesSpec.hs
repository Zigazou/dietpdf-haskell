module PDF.Document.ResourcesSpec
  ( spec
  ) where

import Control.Monad (forM_)

import Data.PDF.PDFDocument (PDFDocument, fromList)
import Data.PDF.PDFObject
  ( PDFObject (PDFEndOfFile, PDFIndirectObject, PDFIndirectObjectWithStream, PDFNumber, PDFReference, PDFVersion)
  , mkPDFDictionary
  )
import Data.PDF.PDFWork (evalPDFWorkT, getReference, getAdditionalGStates, setAdditionalGStates)
import Data.PDF.Resource (Resource (ResFont))
import Data.Set (Set)
import Data.Set qualified as Set

import PDF.Document.Resources (getAllResourceNames, updateWithAdditionalResources)
import PDF.Processing.PDFWork (importObjects)

import Util.Dictionary (mkDictionary)

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

  describe "updateWithAdditionalResources" $ do
    it "adds states to pages and forms while preserving separate resource scopes" $ do
      let states = mkDictionary [("new", mkPDFDictionary [("LW", PDFNumber 2)])]
          resources font = mkPDFDictionary
            [("Font", mkPDFDictionary [("F1", PDFReference font 0)]),
             ("ExtGState", PDFReference 5 0)]
          updated font = mkPDFDictionary
            [("Font", mkPDFDictionary [("F1", PDFReference font 0)]),
             ("ExtGState", mkPDFDictionary
               [("old", PDFReference 6 0), ("new", mkPDFDictionary [("LW", PDFNumber 2)])])]
          document = fromList
            [PDFIndirectObject 1 0 (mkPDFDictionary [("Resources", PDFReference 3 0)]),
             PDFIndirectObjectWithStream 2 0
               (mkDictionary [("Resources", resources 8)]) "content",
             PDFIndirectObject 3 0 (resources 7),
             PDFIndirectObject 5 0 (mkPDFDictionary [("old", PDFReference 6 0)])]
      result <- evalPDFWorkT $ do
        importObjects document
        setAdditionalGStates states
        updateWithAdditionalResources
        page <- getReference (PDFReference 1 0)
        form <- getReference (PDFReference 2 0)
        remaining <- getAdditionalGStates
        return (page, form, remaining)
      result `shouldBe` Right
        (PDFIndirectObject 1 0 (mkPDFDictionary [("Resources", updated 7)]),
         PDFIndirectObjectWithStream 2 0
           (mkDictionary [("Resources", updated 8)]) "content", mempty)
