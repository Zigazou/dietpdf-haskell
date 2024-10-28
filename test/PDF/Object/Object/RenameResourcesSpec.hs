module PDF.Object.Object.RenameResourcesSpec
  ( spec
  )
where

import Control.Monad (forM_)

import Data.Map qualified as Map
import Data.PDF.PDFObject
  (PDFObject (PDFIndirectObject, PDFNull), mkPDFDictionary)
import Data.PDF.Resource
  (Resource (ResExtGState, ResFont, ResPattern, ResXObject))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.TranslationTable (TranslationTable)

import PDF.Object.Object.RenameResources (renameResources)

import Test.Hspec (Spec, describe, it, shouldBe)

objectExamples :: [(Set Int, PDFObject, PDFObject)]
objectExamples =
  [ ( Set.fromList [1]
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
  , ( mempty
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
  , ( mempty
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
  ]

renameTable :: TranslationTable Resource
renameTable = Map.fromList
  [ (ResFont "a", ResFont "0")
  , (ResFont "b", ResFont "1")
  , (ResExtGState "c", ResExtGState "2")
  , (ResPattern "d", ResPattern "3")
  , (ResXObject "e", ResXObject "4")
  ]

spec :: Spec
spec = describe "renameResources" $ forM_ objectExamples $ \(resourcesId, example, expected) ->
  it ("should rename " ++ show example) $ do
    renameResources renameTable resourcesId example `shouldBe` expected
