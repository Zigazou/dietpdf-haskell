module PDF.Document.InvisibleImagesSpec (spec) where

import Control.Monad.State (gets, modify)
import Data.ByteString (ByteString)
import Data.Fallible (Fallible)
import Data.IntMap.Strict qualified as IM
import Data.PDF.PDFDocument (fromList)
import Data.PDF.PDFObject (PDFObject (PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNumber, PDFReference, PDFTrailer), mkPDFDictionary, mkPDFArray)
import Data.PDF.PDFPartition (ppObjectsWithStream)
import Data.PDF.PDFWork (evalPDFWorkT, getReference, putObject)
import Data.PDF.WorkData (wPDF, wSettings)
import Data.PDF.Settings (sOptimizeGFX, OptimizeGFX (DoNotOptimizeGFX))
import PDF.Document.InvisibleImages (removeInvisiblePageImages)
import PDF.Document.MergeVectorStream (mergeVectorStream)
import PDF.Document.Resources (removeUnusedResources)
import PDF.Object.State (getStream)
import PDF.Processing.PDFWork (importObjects, removeUnusedObjects)
import Test.Hspec (Spec, describe, it, shouldBe)
import Util.Dictionary (mkDictionary)

box :: [Double] -> PDFObject
box = mkPDFArray . map PDFNumber

image :: PDFObject
image = PDFIndirectObjectWithStream 5 0 (mkDictionary
  [("Subtype",PDFName "Image"),("ColorSpace",PDFName "DeviceRGB")]) "image"

resources :: PDFObject
resources = mkPDFDictionary [("XObject",mkPDFDictionary [("Im",PDFReference 5 0)])]

page :: PDFObject
page = PDFIndirectObject 1 0 (mkPDFDictionary
  [("Type",PDFName "Page"),("Parent",PDFReference 2 0),("Contents",PDFReference 4 0)])

parent :: PDFObject
parent = PDFIndirectObject 2 0 (mkPDFDictionary
  [("MediaBox",box [0,0,100,100]),("CropBox",box [10,10,90,90]),("Resources",PDFReference 3 0)])

optimize :: PDFObject -> ByteString -> IO (Fallible ByteString)
optimize pageObject bytes = evalPDFWorkT $ do
  importObjects $ fromList [pageObject,parent,PDFIndirectObject 3 0 resources,image]
  removeInvisiblePageImages pageObject (PDFIndirectObjectWithStream 4 0 mempty bytes) >>= getStream

spec :: Spec
spec = describe "Invisible page images" $ do
  it "resolves inherited boxes and indirect resource dictionaries" $ do
    result <- optimize page "/Im Do"
    result `shouldBe` Right ""
  it "preserves images intersecting the inherited CropBox" $ do
    let bytes :: ByteString
        bytes = "20 0 0 20 5 5 cm /Im Do"
    result <- optimize page bytes
    result `shouldBe` Right bytes
  it "uses page resources instead of same-named inherited resources" $ do
    let local = PDFIndirectObject 1 0 (mkPDFDictionary
          [("Parent",PDFReference 2 0),("Resources",mkPDFDictionary [])])
    result <- optimize local "/Im Do"
    result `shouldBe` Right "/Im Do"
  it "preserves pages with missing or malformed bounds" $ do
    result <- optimize (mkPDFDictionary [("Resources",resources)]) "/Im Do"
    result `shouldBe` Right "/Im Do"
    malformed <- optimize (mkPDFDictionary
      [("Parent",PDFReference 2 0),("CropBox",box [0,0,0,0])]) "/Im Do"
    malformed `shouldBe` Right "/Im Do"
  it "terminates on cyclic page inheritance" $ do
    result <- evalPDFWorkT $ do
      let cyclic = PDFIndirectObject 1 0 (mkPDFDictionary [("Parent",PDFReference 1 0)])
      importObjects $ fromList [cyclic]
      removeInvisiblePageImages cyclic (PDFIndirectObjectWithStream 4 0 mempty "/Im Do") >>= getStream
    result `shouldBe` Right "/Im Do"
  it "preserves malformed streams but excludes off-page images in transparency groups" $ do
    result <- optimize page "(unterminated"
    result `shouldBe` Right "(unterminated"
    grouped <- optimize (mkPDFDictionary
      [("Parent",PDFReference 2 0),("Group",mkPDFDictionary [])]) "/Im Do"
    grouped `shouldBe` Right ""
  it "allows the existing resource and object passes to delete removed images" $ do
    result <- evalPDFWorkT $ do
      importObjects $ fromList
        [PDFTrailer (mkPDFDictionary [("Root",PDFReference 1 0)]),page,parent,
         PDFIndirectObject 3 0 resources,image,
         PDFIndirectObjectWithStream 4 0 mempty "/Im Do"]
      original <- getReference (PDFReference 4 0)
      removeInvisiblePageImages page original >>= putObject
      removeUnusedResources
      removeUnusedObjects
      gets (IM.keys . ppObjectsWithStream . wPDF)
    result `shouldBe` Right [4]
  it "analyzes occlusion across merged content streams without editing originals" $ do
    result <- evalPDFWorkT $ do
      let first = PDFIndirectObjectWithStream 6 0 mempty "20 0 0 20 20 20 cm /Im Do"
          second = PDFIndirectObjectWithStream 7 0 mempty "-1 -1 3 3 re f"
      importObjects $ fromList [page,parent,PDFIndirectObject 3 0 resources,image,first,second]
      merged <- mergeVectorStream (mkPDFArray [PDFReference 6 0,PDFReference 7 0])
      optimized <- removeInvisiblePageImages page merged >>= getStream
      original <- getReference (PDFReference 6 0) >>= getStream
      return (optimized,original)
    result `shouldBe` Right ("20 0 0 20 20 20 cm -1 -1 3 3 re f","20 0 0 20 20 20 cm /Im Do")

  it "honors the graphics optimization setting" $ do
    result <- evalPDFWorkT $ do
      importObjects $ fromList [page,parent,PDFIndirectObject 3 0 resources,image]
      modify (\work -> work { wSettings = (wSettings work) { sOptimizeGFX = DoNotOptimizeGFX } })
      removeInvisiblePageImages page (PDFIndirectObjectWithStream 4 0 mempty "/Im Do") >>= getStream
    result `shouldBe` Right "/Im Do"
  it "does not interpret encoded content as graphics" $ do
    result <- evalPDFWorkT $ do
      importObjects $ fromList [page,parent,PDFIndirectObject 3 0 resources,image]
      removeInvisiblePageImages page (PDFIndirectObjectWithStream 4 0
        (mkDictionary [("Filter",PDFName "UnknownDecode")]) "/Im Do") >>= getStream
    result `shouldBe` Right "/Im Do"
  it "does not use embedded image alpha as proof of opaque coverage" $ do
    result <- evalPDFWorkT $ do
      let alpha = PDFIndirectObjectWithStream 5 0 (mkDictionary
            [("Subtype",PDFName "Image"),("ColorSpace",PDFName "DeviceRGB"),
             ("SMaskInData",PDFNumber 1)]) "image"
      importObjects $ fromList [page,parent,PDFIndirectObject 3 0 resources,alpha]
      removeInvisiblePageImages page (PDFIndirectObjectWithStream 4 0 mempty
        "20 0 0 20 20 20 cm /Im Do /Im Do") >>= getStream
    result `shouldBe` Right "20 0 0 20 20 20 cm /Im Do /Im Do"

  it "retains covered images inside page transparency groups" $ do
    let bytes = "20 0 0 20 20 20 cm /Im Do -1 -1 3 3 re f /Im Do"
    result <- optimize (mkPDFDictionary
      [("Parent",PDFReference 2 0),("Group",mkPDFDictionary [])]) bytes
    result `shouldBe` Right bytes
  it "removes the tagged off-page image placement from the FPGA page" $ do
    let grouped = mkPDFDictionary
          [("Resources",resources),("MediaBox",box [0,0,793.700787,446.456693]),
           ("Group",mkPDFDictionary [("S",PDFName "Transparency")])]
    result <- optimize grouped
      "/Figure<</MCID 1>>BDC q 0.028 90.992 793.615 355.464 re W* n q 793.616 0 0 142.554 -232.753 687.769 cm /Im Do Q Q EMC"
    result `shouldBe` Right
      "/Figure<</MCID 1>>BDC q .028 90.992 793.615 355.464 re W* n q 793.616 0 0 142.554 -232.753 687.769 cm Q Q EMC"
