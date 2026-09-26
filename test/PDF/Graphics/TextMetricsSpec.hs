module PDF.Graphics.TextMetricsSpec (spec) where

import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as Map
import Data.PDF.PDFObject
  (PDFObject (PDFIndirectObject, PDFName, PDFNumber, PDFReference), mkPDFDictionary, mkPDFArray)
import PDF.Graphics.TextMetrics
  (TextResources (textFonts, textExtFonts), ExtFont (UnchangedFont, UnknownFont), buildTextResources, glyphWidths, serializedNumber)
import Test.Hspec (Spec, describe, it, shouldBe)
import Util.Dictionary (mkDictionary)

spec :: Spec
spec = describe "TextMetrics" $ do
  let widths ns = mkPDFArray (map PDFNumber ns)
      simple = mkPDFDictionary
        [("Subtype", PDFName "TrueType"), ("FirstChar", PDFNumber 32),
         ("LastChar", PDFNumber 33), ("Widths", widths [250, 600]),
         ("FontDescriptor", mkPDFDictionary [("MissingWidth", PDFNumber 400)])]
      composite encoding entries = mkPDFDictionary
        [("Subtype", PDFName "Type0"), ("Encoding", encoding),
         ("DescendantFonts", mkPDFArray [mkPDFDictionary
           (("Subtype", PDFName "CIDFontType2") : entries)])]
      resources font = Just (mkDictionary [("Font", mkPDFDictionary [("F", font)])])
      decode objects font bytes = Map.lookup "F" (textFonts (buildTextResources objects (resources font)))
                                  >>= (`glyphWidths` bytes)

  it "uses simple-font code widths, word-space flags and MissingWidth" $
    decode IM.empty simple " !A" `shouldBe` Just [(250,True),(600,False),(400,False)]
  it "resolves indirect categories, fonts, widths and width values" $ do
    let objects = IM.fromList
          [(1, PDFIndirectObject 1 0 (mkPDFDictionary [("F", PDFReference 2 0)])),
           (2, PDFIndirectObject 2 0 (mkPDFDictionary
             [("Subtype",PDFName "Type1"),("FirstChar",PDFNumber 65),
              ("LastChar",PDFNumber 65),("Widths",PDFReference 3 0)])),
           (3, PDFIndirectObject 3 0 (mkPDFArray [PDFReference 4 0])),
           (4, PDFIndirectObject 4 0 (PDFNumber 500))]
        context = buildTextResources objects (Just (mkDictionary [("Font",PDFReference 1 0)]))
    (Map.lookup "F" (textFonts context) >>= (`glyphWidths` "A")) `shouldBe` Just [(500,False)]
    (Map.lookup "F" (textFonts context) >>= (`glyphWidths` "B")) `shouldBe` Nothing
  it "decodes both CID W syntaxes and DW without applying Tw to CID 32" $
    decode IM.empty (composite (PDFName "Identity-H")
      [("W",mkPDFArray [PDFNumber 1,widths [500,600],PDFNumber 32,PDFNumber 40,PDFNumber 250]),
       ("DW",PDFNumber 700)]) "\x00\x01\x00\x02\x00\x20\x00\x50"
      `shouldBe` Just [(500,False),(600,False),(250,False),(700,False)]
  it "uses the CID default width of 1000" $
    decode IM.empty (composite (PDFName "Identity-H") []) "\x00\x01"
      `shouldBe` Just [(1000,False)]
  it "rejects incomplete codes, vertical writing and unsupported CMaps" $ do
    decode IM.empty (composite (PDFName "Identity-H") []) "a" `shouldBe` Nothing
    decode IM.empty (composite (PDFName "Identity-V") []) "ab" `shouldBe` Nothing
    decode IM.empty (composite (PDFName "UniJIS-UTF16-H") []) "ab" `shouldBe` Nothing
  it "rejects overlapping, malformed or out-of-range CID widths" $ do
    let bad entries = decode IM.empty (composite (PDFName "Identity-H") [("W",mkPDFArray entries)]) "\x00\x01"
    bad [PDFNumber 1,PDFNumber 5,PDFNumber 500,PDFNumber 3,widths [600]] `shouldBe` Nothing
    bad [PDFNumber 1,PDFNumber 5] `shouldBe` Nothing
    bad [PDFNumber 65535,widths [500,600]] `shouldBe` Nothing
  it "does not guess standard-font or Type3 widths or follow cyclic references" $ do
    decode IM.empty (mkPDFDictionary [("Subtype",PDFName "Type1"),("BaseFont",PDFName "Helvetica")]) "A" `shouldBe` Nothing
    decode IM.empty (mkPDFDictionary [("Subtype",PDFName "Type3")]) "A" `shouldBe` Nothing
    decode (IM.singleton 1 (PDFIndirectObject 1 0 (PDFReference 1 0))) (PDFReference 1 0) "A" `shouldBe` Nothing
  it "distinguishes a gs without Font from an unresolved gs" $ do
    let context = buildTextResources IM.empty (Just (mkDictionary [("ExtGState",mkPDFDictionary
          [("Keep",mkPDFDictionary []),("Bad",PDFReference 99 0)])]))
    textExtFonts context `shouldBe` Map.fromList [("Keep",UnchangedFont),("Bad",UnknownFont)]
  it "uses exact serialized decimals and rejects nonfinite inputs" $ do
    serializedNumber 0.1 `shouldBe` Just (1 / 10)
    serializedNumber (-0.1234567) `shouldBe` Just (-123457 / 1000000)
    serializedNumber (0 / 0) `shouldBe` Nothing
    serializedNumber (1 / 0) `shouldBe` Nothing
