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
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Util.Dictionary (mkDictionary)

spec :: Spec
spec = describe "unused resource removal" $ do
  it "discards unused indirect resources and their dependency chains" $ do
    result <- optimize "/Used Do"
    result `shouldEqualContents` Right ([4, 7], [1, 2, 6])
  it "preserves the Linux cover's pattern color space in form resources" $ do
    let resources = mkPDFDictionary
          [("ColorSpace", PDFReference 2 0), ("Pattern", PDFReference 3 0)]
        form = PDFIndirectObjectWithStream 1 0
          (mkDictionary [("Subtype", PDFName "Form"), ("Resources", resources)])
          "q /R9 cs /R15 scn 0 0 100 100 re f Q"
    result <- evalPDFWorkT $ do
      importObjects $ fromList
        [ form
        , PDFIndirectObject 2 0 (mkPDFDictionary
            [("R9", mkPDFArray [PDFName "Pattern"]),
             ("Unused", PDFName "DeviceRGB")])
        , PDFIndirectObject 3 0 (mkPDFDictionary [("R15", PDFReference 4 0)])
        ]
      removeUnusedResources
      getReference (PDFReference 1 0)
    result `shouldEqualContents` Right (PDFIndirectObjectWithStream 1 0
      (mkDictionary [("Subtype", PDFName "Form"),
        ("Resources", mkPDFDictionary
          [("ColorSpace", mkPDFDictionary [("R9", mkPDFArray [PDFName "Pattern"])]),
           ("Pattern", mkPDFDictionary [("R15", PDFReference 4 0)])])])
      "q /R9 cs /R15 scn 0 0 100 100 re f Q")

  it "preserves resources when a content stream cannot be parsed" $ do
    result <- optimize "(unterminated"
    result `shouldEqualContents` Right ([4, 7, 8], [1, 2, 3, 5, 6, 9])
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
    result `shouldEqualContents` Right
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
    result `shouldEqualContents` Right (PDFIndirectObject 1 0 (mkPDFDictionary
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
    result `shouldEqualContents` Right page

  it "leaves resources unchanged when default appearances need analysis" $ do
    let page = PDFIndirectObject 1 0 (mkPDFDictionary
          [("DA", PDFName "Appearance"),
           ("Resources", mkPDFDictionary [("Font", mkPDFDictionary
             [("Keep", PDFReference 7 0)])])])
    result <- evalPDFWorkT $ do
      importObjects (fromList [page])
      removeUnusedResources
      getReference (PDFReference 1 0)
    result `shouldEqualContents` Right page

  it "handles cyclic content references and retains resources on parse failure" $ do
    let page = PDFIndirectObject 1 0 (mkPDFDictionary
          [("Resources", mkPDFDictionary [("Font", mkPDFDictionary
            [("Keep", PDFReference 7 0)])]), ("Contents", PDFReference 2 0)])
    result <- evalPDFWorkT $ do
      importObjects $ fromList
        [ page
        , PDFIndirectObject 2 0 (mkPDFArray [PDFReference 3 0])
        , PDFIndirectObject 3 0 (mkPDFArray [PDFReference 2 0, PDFReference 4 0])
        , PDFIndirectObjectWithStream 4 0 mempty "(unterminated"
        ]
      removeUnusedResources
      getReference (PDFReference 1 0)
    result `shouldEqualContents` Right page

  it "preserves unknown categories and array ProcSets while pruning known categories" $ do
    let categories = mkPDFDictionary
          [("Custom", mkPDFDictionary [("Keep", PDFReference 7 0)]),
           ("ProcSet", mkPDFArray [PDFName "PDF"]),
           ("Font", mkPDFDictionary [("Unused", PDFReference 8 0)])]
        page = PDFIndirectObject 1 0 (mkPDFDictionary [("Resources", categories)])
    result <- evalPDFWorkT $ do
      importObjects (fromList [page])
      removeUnusedResources
      getReference (PDFReference 1 0)
    result `shouldEqualContents` Right (PDFIndirectObject 1 0 (mkPDFDictionary
      [("Resources", mkPDFDictionary
        [("Custom", mkPDFDictionary [("Keep", PDFReference 7 0)]),
         ("ProcSet", mkPDFArray [PDFName "PDF"]),
         ("Font", mkPDFDictionary [])])]))

  it "does not treat image samples as graphics resource references" $ do
    let resources = mkPDFDictionary [("Font", mkPDFDictionary
          [("Pixels", PDFReference 7 0)])]
        page = PDFIndirectObject 1 0 (mkPDFDictionary [("Resources", resources)])
    result <- evalPDFWorkT $ do
      importObjects $ fromList
        [ page
        , PDFIndirectObjectWithStream 2 0
            (mkDictionary [("Subtype", PDFName "Image")]) "/Pixels 12 Tf"
        ]
      removeUnusedResources
      getReference (PDFReference 1 0)
    result `shouldEqualContents` Right (PDFIndirectObject 1 0 (mkPDFDictionary
      [("Resources", mkPDFDictionary [("Font", mkPDFDictionary [])])]))

  it "still validates images referenced as mandatory page content" $ do
    let page = PDFIndirectObject 1 0 (mkPDFDictionary
          [("Resources", mkPDFDictionary [("Font", mkPDFDictionary
            [("Keep", PDFReference 7 0)])]), ("Contents", PDFReference 2 0)])
    result <- evalPDFWorkT $ do
      importObjects $ fromList
        [ page
        , PDFIndirectObjectWithStream 2 0
            (mkDictionary [("Subtype", PDFName "Image")]) "(unterminated"
        ]
      removeUnusedResources
      getReference (PDFReference 1 0)
    result `shouldEqualContents` Right page

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
    result `shouldEqualContents` Right [2, 3]

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

-- PDFObject equality ignores the contents of indirect objects. Compare their
-- full representation so resource dictionary regressions cannot pass silently.
shouldEqualContents :: Show a => a -> a -> Expectation
shouldEqualContents actual expected = show actual `shouldBe` show expected
