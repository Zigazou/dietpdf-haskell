module PDF.Graphics.OptimizeSpec (spec) where

import Data.PDF.PDFDocument (fromList)
import Data.PDF.PDFObject
  (PDFObject (PDFIndirectObject, PDFName, PDFNumber, PDFReference), mkPDFDictionary, mkPDFArray)
import Data.PDF.PDFWork (evalPDFWorkT, setTranslationTable)
import Data.PDF.Resource (Resource (ResFont))
import Data.Map.Strict qualified as Map
import PDF.Graphics.Optimize (optimizeGFXWithTextState)
import PDF.Processing.PDFWork (importObjects)
import Test.Hspec (Spec, describe, it, shouldBe)
import Util.Dictionary (mkDictionary)

spec :: Spec
spec = describe "resource-aware graphics optimization" $ do
  it "resolves indirect font metrics using the renamed resource keys" $ do
    result <- evalPDFWorkT $ do
      importObjects $ fromList [PDFIndirectObject 5 0 (mkPDFDictionary
        [("Subtype",PDFName "TrueType"),("FirstChar",PDFNumber 65),("LastChar",PDFNumber 66),
         ("Widths",mkPDFArray [PDFNumber 500,PDFNumber 600])])]
      setTranslationTable (Map.singleton (ResFont "LongName") (ResFont "F"))
      optimizeGFXWithTextState False
        (Just (mkDictionary [("Font",mkPDFDictionary [("F",PDFReference 5 0)])]))
        "BT /LongName 10 Tf 10 20 Td (A) Tj 1 0 0 1 15 20 Tm (B) Tj ET"
    result `shouldBe` Right "BT/F 10 Tf 10 20 Td(AB)Tj ET"
