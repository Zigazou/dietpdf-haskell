module PDF.Document.ResourceContextSpec (spec) where

import Data.IntMap.Strict qualified as IM
import Data.PDF.PDFObject
  (PDFObject (PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFReference),
   getObjectNumber, mkPDFArray, mkPDFDictionary)
import Data.Maybe (fromJust)
import PDF.Document.ResourceContext (buildStreamResources)
import Test.Hspec (Spec, describe, it, shouldBe)
import Util.Dictionary (mkDictionary)

spec :: Spec
spec = describe "ResourceContext" $ do
  let ref n = PDFReference n 0
      indirect n fields = PDFIndirectObject n 0 (mkPDFDictionary fields)
      stream n = PDFIndirectObjectWithStream n 0 mempty ""
      page n resources contents = indirect n
        [("Type", PDFName "Page"), ("Resources", resources), ("Contents", contents)]
      font n = mkPDFDictionary [("Font", mkPDFDictionary [("F1", ref n)])]
      fontDict n = mkDictionary [("Font", mkPDFDictionary [("F1", ref n)])]
      form n fields = PDFIndirectObjectWithStream n 0
        (mkDictionary (("Subtype", PDFName "Form") : fields)) ""
      withForm n fontNumber = mkPDFDictionary
        [("Font", mkPDFDictionary [("F1", ref fontNumber)]),
         ("XObject", mkPDFDictionary [("Fm", ref n)])]
      build xs = buildStreamResources $ IM.fromList
        [(fromJust (getObjectNumber x), x) | x <- xs]

  it "resolves inherited indirect resources and indirect content arrays" $ do
    let result = build
          [indirect 1 [("Type", PDFName "Page"), ("Parent", ref 2), ("Contents", ref 4)],
           indirect 2 [("Resources", ref 3)], PDFIndirectObject 3 0 (font 9),
           PDFIndirectObject 4 0 (mkPDFArray [ref 5, ref 6]), stream 5, stream 6]
    result `shouldBe` IM.fromList [(5, Just (fontDict 9)), (6, Just (fontDict 9))]

  it "preserves an explicitly empty page dictionary over inherited resources" $ do
    build [indirect 1 [("Type", PDFName "Page"), ("Parent", ref 2),
                       ("Resources", mkPDFDictionary []), ("Contents", ref 5)],
           indirect 2 [("Resources", font 9)], stream 5]
      `shouldBe` IM.singleton 5 (Just mempty)

  it "keeps equal shared contexts and makes conflicting contexts permanently unknown" $ do
    build [page 1 (font 9) (ref 5), page 2 (font 9) (ref 5), stream 5]
      `shouldBe` IM.singleton 5 (Just (fontDict 9))
    build [page 1 (font 9) (ref 5), page 2 (font 10) (ref 5),
           page 3 (font 9) (ref 5), stream 5]
      `shouldBe` IM.singleton 5 Nothing

  it "marks a form with conflicting caller resources as unknown" $ do
    let result = build [page 1 (withForm 5 9) (ref 7),
                        page 2 (withForm 5 10) (ref 8), stream 7, stream 8,
                        form 5 []]
    IM.lookup 5 result `shouldBe` Just Nothing

  it "uses a form's own indirect resources independently of its callers" $ do
    let result = build [page 1 (withForm 5 9) (ref 7),
                        page 2 (withForm 5 10) (ref 8), stream 7, stream 8,
                        form 5 [("Resources", ref 3)], PDFIndirectObject 3 0 (font 11)]
    IM.lookup 5 result `shouldBe` Just (Just (fontDict 11))

  it "terminates on recursive forms and visits nested forms" $ do
    let resources = mkPDFDictionary [("XObject", mkPDFDictionary [("Fm", ref 6)])]
        result = build [page 1 (withForm 5 9) (ref 7), stream 7,
                        form 5 [("Resources", resources)], form 6 []]
    IM.lookup 5 result `shouldBe` IM.lookup 6 result
    IM.keys result `shouldBe` [5,6,7]

  it "rejects missing resources, reference cycles and generation mismatches" $ do
    let result = build
          [page 1 (ref 3) (ref 7), indirect 3 [("Unused", ref 3)], stream 7,
           page 2 (PDFReference 4 1) (ref 8), PDFIndirectObject 4 0 (font 9), stream 8,
           indirect 5 [("Type", PDFName "Page"), ("Parent", ref 5), ("Contents", ref 9)],
           stream 9, page 6 (ref 10) (ref 11), PDFIndirectObject 10 0 (ref 10), stream 11]
    IM.lookup 8 result `shouldBe` Just Nothing
    IM.lookup 9 result `shouldBe` Just Nothing
    IM.lookup 11 result `shouldBe` Just Nothing

  it "does not replace malformed local resources with inherited resources" $ do
    build [indirect 1 [("Type", PDFName "Page"), ("Parent", ref 2),
                       ("Resources", PDFName "invalid"), ("Contents", ref 5)],
           indirect 2 [("Resources", font 9)], stream 5]
      `shouldBe` IM.singleton 5 Nothing

  it "rejects a stream reference with the wrong generation" $ do
    build [page 1 (font 9) (PDFReference 5 1), stream 5]
      `shouldBe` IM.empty

  it "leaves unowned streams without context" $ do
    build [stream 5, form 6 []] `shouldBe` IM.empty
