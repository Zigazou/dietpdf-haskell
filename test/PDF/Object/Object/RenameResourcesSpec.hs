module PDF.Object.Object.RenameResourcesSpec
  ( spec
  )
where

import Control.Monad (forM_)

import Data.Map qualified as Map
import Data.PDF.PDFDocument (PDFDocument, fromList)
import Data.PDF.PDFObject
  ( PDFObject (PDFIndirectObject, PDFIndirectObjectWithStream, PDFNull, PDFReference)
  , mkPDFDictionary
  )
import Data.PDF.PDFWork (evalPDFWorkT)
import Data.PDF.Resource
  (Resource (ResExtGState, ResFont, ResPattern, ResXObject))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.TranslationTable (TranslationTable)

import PDF.Object.Object.RenameResources (containsResources, renameResources)
import PDF.Processing.PDFWork (importObjects)

import Test.Hspec (Spec, describe, it, shouldBe)

import Util.Dictionary (mkDictionary)

objectExamples :: [(Int, Set Int, PDFObject, PDFObject)]
objectExamples =
  [ ( 0, mempty, PDFNull, PDFNull )
  , ( 1
    , Set.fromList [1]
    , PDFIndirectObject 1 0 (mkPDFDictionary
        [ ( "Font"
          , mkPDFDictionary [("a", PDFNull)]
          )
        ])
    , PDFIndirectObject 1 0 (mkPDFDictionary
        [ ( "Font"
          , mkPDFDictionary [("0", PDFNull)]
          )
        ])
    )
  , ( 2
    , mempty
    , PDFIndirectObject 1 0 (mkPDFDictionary
        [ ( "Font"
          , mkPDFDictionary [("a", PDFNull)]
          )
        ])
    , PDFIndirectObject 1 0 (mkPDFDictionary
        [ ( "Font"
          , mkPDFDictionary [("a", PDFNull)]
          )
        ])
    )
  , ( 3
    , mempty
    , mkPDFDictionary
        [ ( "Resources"
          , mkPDFDictionary
            [ ( "Font"
              , mkPDFDictionary [("a", PDFNull)]
              )
            ]
          )
        ]
    , mkPDFDictionary
        [ ( "Resources"
          , mkPDFDictionary
            [ ( "Font"
              , mkPDFDictionary [("0", PDFNull)]
              )
            ]
          )
        ]
    )
  , ( 4
    , mempty
    , PDFIndirectObject 1 0
        ( mkPDFDictionary
          [ ( "Resources"
            , mkPDFDictionary
              [ ( "Font"
                , mkPDFDictionary [("a", PDFNull)]
                )
              ]
            )
          ]
        )
    , PDFIndirectObject 1 0
        ( mkPDFDictionary
          [ ( "Resources"
            , mkPDFDictionary
              [ ( "Font"
                , mkPDFDictionary [("a", PDFNull)]
                )
              ]
            )
          ]
        )
    )
  , ( 5
    , mempty
    , PDFIndirectObjectWithStream 1 0
        ( mkDictionary
          [ ( "Resources"
            , mkPDFDictionary
              [ ( "Font"
                , mkPDFDictionary [("a", PDFNull)]
                )
              ]
            )
          ]
        )
        "dummy"
    , PDFIndirectObjectWithStream 1 0
        ( mkDictionary
          [ ( "Resources"
            , mkPDFDictionary
              [ ( "Font"
                , mkPDFDictionary [("0", PDFNull)]
                )
              ]
            )
          ]
        )
        "dummy"
    )
  , ( 6
    , Set.fromList [1]
    , PDFIndirectObject 1 0 (mkPDFDictionary
        [ ( "Font"
          , mkPDFDictionary [("a", PDFReference 10 0)]
          )
        ])
    , PDFIndirectObject 1 0 (mkPDFDictionary
        [ ( "Font"
          , mkPDFDictionary [("0", PDFReference 10 0)]
          )
        ])
    )
  , ( 7
    , Set.fromList [1]
    , PDFIndirectObject 1 0 (mkPDFDictionary [("a", PDFReference 10 0)])
    , PDFIndirectObject 1 0 (mkPDFDictionary [("0", PDFReference 10 0)])
    )
  ]

renameTable :: TranslationTable Resource
renameTable = Map.fromList
  [ (ResFont "a", ResFont "0")
  , (ResFont "b", ResFont "1")
  , (ResExtGState "c", ResExtGState "2")
  , (ResPattern "d", ResPattern "3")
  , (ResXObject "e", ResXObject "4")
  ]

containsResourcesExamples :: [(Int, PDFDocument, Set Int)]
containsResourcesExamples =
  [ ( 0, mempty, mempty )
  , ( 1
    , fromList
        [ mkPDFDictionary
            [ ( "Resources"
              , mkPDFDictionary
                  [ ( "ColorSpace", PDFReference 1 0 ) ]
              )
            ]
        ]
    , Set.singleton 1
    )
  , ( 2
    , fromList
        [ PDFIndirectObject
            1
            0
            (mkPDFDictionary
              [ ( "Resources"
                , mkPDFDictionary
                    [ ( "ColorSpace", PDFReference 2 0 ) ]
                )
              ]
            )
        , PDFIndirectObject 2 0 (mkPDFDictionary [ ( "x", PDFNull) ])
        ]
    , Set.fromList [1, 2]
    )
  , ( 3
    , fromList
        [ PDFIndirectObject 1 0 (mkPDFDictionary [("Resources", PDFReference 2 0)])
        , PDFIndirectObject
            2
            0
            (mkPDFDictionary
              [ ("ColorSpace", PDFReference 3 0)
              , ("ExtGState", PDFReference 4 0)
              ]
            )
        , PDFIndirectObject 3 0 (mkPDFDictionary [ ( "x", PDFNull) ])
        , PDFIndirectObject 4 0 (mkPDFDictionary [ ( "y", PDFNull) ])
        , PDFIndirectObject 5 0 (mkPDFDictionary [ ( "z", PDFNull) ])
        , PDFIndirectObject 6 0 (mkPDFDictionary [ ( "t", PDFNull) ])
        ]
    , Set.fromList [1, 2, 3, 4]
    )
  , ( 4
    , fromList
        [ PDFIndirectObject 1 0 (mkPDFDictionary [("Resources", PDFReference 2 0)])
        , PDFIndirectObject
            2
            0
            (mkPDFDictionary
              [ ("ColorSpace", PDFReference 4 0)
              , ("ExtGState", PDFReference 5 0)
              ]
            )
        , PDFIndirectObject
            3
            0
            (mkPDFDictionary
              [ ("ColorSpace", PDFReference 5 0)
              , ("ExtGState", PDFReference 6 0)
              ]
            )
        , PDFIndirectObject 4 0 (mkPDFDictionary [ ( "x", PDFNull) ])
        , PDFIndirectObject 5 0 (mkPDFDictionary [ ( "y", PDFNull) ])
        , PDFIndirectObject 6 0 (mkPDFDictionary [ ( "z", PDFNull) ])
        , PDFIndirectObject 7 0 (mkPDFDictionary [ ( "t", PDFNull) ])
        ]
    , Set.fromList [1, 2, 4, 5]
    )
  ]

spec :: Spec
spec = do
  describe "renameResources" $
    forM_ objectExamples $ \(identifier, resourcesId, example, expected) ->
      it ("should rename example " ++ show identifier) $ do
        renameResources renameTable resourcesId example `shouldBe` expected

  describe "containsResources" $ do
    forM_ containsResourcesExamples $ \(identifier, example, expected) ->
      it ("should find object containing resources in example " ++ show identifier)
        $   evalPDFWorkT (importObjects example >> containsResources example)
        >>= \case
          Right result -> result `shouldBe` expected
          Left _failed -> fail "Failed to use containsResources"
