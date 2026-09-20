module PDF.Document.UnusedResourcesSpec (spec) where

import Data.PDF.PDFDocument (fromList)
import Data.PDF.PDFObject (PDFObject (PDFTrailer, PDFReference, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNumber), mkPDFDictionary, mkPDFArray)
import Data.PDF.PDFWork (evalPDFWorkT, getReference)
import Data.ByteString (ByteString)
import Data.Fallible (Fallible)
import Data.PDF.WorkData (wPDF)
import Data.PDF.PDFPartition (ppObjectsWithStream, ppObjectsWithoutStream)
import Data.IntMap qualified as IM
import Control.Monad.State (gets)
import PDF.Document.Resources (removeUnusedResources)
import PDF.Processing.PDFWork (importObjects, removeUnusedObjects)
import Test.Hspec (Spec, describe, it, shouldBe)
import Util.Dictionary (mkDictionary)

spec :: Spec
spec = describe "unused resource removal" $ do
  it "discards unused indirect resources and their dependency chains" $ do
    result <- optimize "/Used Do"
    result `shouldBe` Right ([4, 7], [1, 2, 6])
  it "preserves resources when a content stream cannot be parsed" $ do
    result <- optimize "(unterminated"
    result `shouldBe` Right ([4, 7, 8], [1, 2, 3, 5, 6, 9])
  it "retains resources used by forms sharing an inherited resource dictionary" $ do
    let resources = mkPDFDictionary [("XObject", mkPDFDictionary
                      [("Used", PDFReference 7 0), ("Unused", PDFReference 8 0)])]
        document = fromList
          [ PDFIndirectObject 1 0 (mkPDFDictionary [("Resources", PDFReference 3 0)])
          , PDFIndirectObject 2 0 (mkPDFDictionary [("Resources", PDFReference 3 0)])
          , PDFIndirectObject 3 0 resources
          , PDFIndirectObjectWithStream 4 0
              (mkDictionary [("Subtype", PDFName "Form")]) "/Used Do"
          ]
        expected = mkPDFDictionary [("Resources", mkPDFDictionary
                     [("XObject", mkPDFDictionary [("Used", PDFReference 7 0)])])]
    result <- evalPDFWorkT $ do
      importObjects document
      removeUnusedResources
      (,) <$> getReference (PDFReference 1 0) <*> getReference (PDFReference 2 0)
    result `shouldBe` Right
      (PDFIndirectObject 1 0 expected, PDFIndirectObject 2 0 expected)

  it "preserves implicit default color spaces and names in inline images" $ do
    let categories = mkPDFDictionary [("ColorSpace", mkPDFDictionary
          [("DefaultRGB", PDFReference 7 0), ("Custom", PDFReference 8 0),
           ("Unused", PDFReference 9 0)])]
        page = PDFIndirectObject 1 0 (mkPDFDictionary
          [("Resources", categories), ("Contents", PDFReference 2 0)])
    result <- evalPDFWorkT $ do
      importObjects $ fromList
        [page, PDFIndirectObjectWithStream 2 0 mempty
          "BI /W 1 /H 1 /BPC 8 /CS /Custom ID x EI"]
      removeUnusedResources
      getReference (PDFReference 1 0)
    result `shouldBe` Right (PDFIndirectObject 1 0 (mkPDFDictionary
      [("Resources", mkPDFDictionary [("ColorSpace", mkPDFDictionary
        [("DefaultRGB", PDFReference 7 0), ("Custom", PDFReference 8 0)])]),
       ("Contents", PDFReference 2 0)]))

  it "preserves resources for unsupported filters in indirect content arrays" $ do
    let page = PDFIndirectObject 1 0 (mkPDFDictionary
          [("Resources", mkPDFDictionary [("Font", mkPDFDictionary
            [("Keep", PDFReference 7 0)])]), ("Contents", PDFReference 2 0)])
    result <- evalPDFWorkT $ do
      importObjects $ fromList
        [ page
        , PDFIndirectObject 2 0 (mkPDFArray [PDFReference 3 0])
        , PDFIndirectObjectWithStream 3 0
            (mkDictionary [("Filter", PDFName "UnknownDecode")]) "q Q"
        ]
      removeUnusedResources
      getReference (PDFReference 1 0)
    result `shouldBe` Right page

  it "keeps indirect stream metadata referenced by the original encoded object" $ do
    result <- evalPDFWorkT $ do
      importObjects $ fromList
        [ PDFTrailer (mkPDFDictionary [("Root", PDFReference 1 0)])
        , PDFIndirectObjectWithStream 1 0
            (mkDictionary [("Length", PDFReference 2 0),
                           ("Filter", PDFReference 3 0)]) "q Q"
        , PDFIndirectObject 2 0 (PDFNumber 3)
        , PDFIndirectObject 3 0 (PDFName "UnknownDecode")
        ]
      removeUnusedObjects
      gets (IM.keys . ppObjectsWithoutStream . wPDF)
    result `shouldBe` Right [2, 3]

 where
  optimize :: ByteString -> IO (Fallible ([Int], [Int]))
  optimize content = evalPDFWorkT $ do
    let dictionary = mkPDFDictionary
    importObjects $ fromList
      [ PDFTrailer (dictionary [("Root", PDFReference 1 0)])
      , PDFIndirectObject 1 0 (dictionary [("Pages", PDFReference 2 0)])
      , PDFIndirectObject 2 0 (dictionary
          [("Contents", PDFReference 4 0), ("Resources", PDFReference 3 0)])
      , PDFIndirectObject 3 0 (dictionary [("XObject", PDFReference 5 0)])
      , PDFIndirectObject 5 0 (dictionary
          [("Used", PDFReference 7 0), ("Unused", PDFReference 8 0)])
      , PDFIndirectObjectWithStream 4 0 mempty content
      , PDFIndirectObjectWithStream 7 0
          (mkDictionary [("Subtype", PDFName "Image"), ("ColorSpace", PDFReference 6 0)]) "image"
      , PDFIndirectObject 6 0 (PDFName "DeviceRGB")
      , PDFIndirectObjectWithStream 8 0
          (mkDictionary [("Subtype", PDFName "Image"), ("ColorSpace", PDFReference 9 0)]) "image"
      , PDFIndirectObject 9 0 (PDFName "DeviceGray")
      , PDFIndirectObject 10 0 (dictionary [("Cycle", PDFReference 11 0)])
      , PDFIndirectObject 11 0 (dictionary [("Cycle", PDFReference 10 0)])
      ]
    removeUnusedResources
    removeUnusedObjects
    gets (\work -> (IM.keys (ppObjectsWithStream (wPDF work)),
                    IM.keys (ppObjectsWithoutStream (wPDF work))))
