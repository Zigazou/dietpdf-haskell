module PDF.Processing.RepeatedFormFragmentsSpec (spec) where

import Control.Monad ((>=>))
import Control.Monad.State (gets, modify')
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Fallible (Fallible)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as Map
import Data.PDF.PDFDocument (fromList)
import Data.PDF.PDFObject (PDFObject (PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNumber, PDFReference), mkPDFArray, mkPDFDictionary)
import Data.PDF.PDFPartition (PDFPartition (ppObjectsWithStream, ppObjectsWithoutStream))
import Data.PDF.PDFWork (evalPDFWorkT)
import Data.PDF.Settings (Settings (sCompressor, sOptimizeGFX), UseCompressor (UseDeflate), OptimizeGFX (DoNotOptimizeGFX))
import Data.PDF.WorkData (WorkData (wPDF, wSettings))
import PDF.Object.Object (fromPDFObject)
import PDF.Object.Object.Properties (getValueForKey)
import PDF.Object.State (getStream)
import PDF.Processing.Filter (filterOptimize)
import Data.PDF.OptimizationType (OptimizationType (GfxOptimization))
import Data.PDF.PDFWork (modifyIndirectObjectsP)
import PDF.Processing.PDFWork (importObjects)
import PDF.Processing.RepeatedFormFragments (repeatedFormFragments)
import PDF.Processing.Unfilter (unfilter)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

-- Irregular geometry remains expensive after compression, unlike repeated
-- rectangles that the stream compressor already handles very effectively.
fragment :: ByteString
fragment = "0.2 0.4 0.7 rg " <> BS.concat
  [BC.pack (unwords [show ((n * 7919) `mod` 997), show ((n * 3571) `mod` 991),
                    show (n `mod` 37 + 1), show (n `mod` 41 + 1), "re f "])
  | n <- [1 :: Int .. 250]]

page :: Int -> ByteString -> [(ByteString, PDFObject)] -> [PDFObject]
page n content extra =
  [PDFIndirectObject n 0 (mkPDFDictionary
    ([("Type", PDFName "Page"), ("Contents", PDFReference (n + 1) 0)] ++ extra)),
   PDFIndirectObjectWithStream (n + 1) 0 Map.empty content]

document :: [PDFObject]
document = concat [page n ("q " <> fragment <> "Q") [] | n <- [1,3,5,7]]

runPass :: Bool -> [PDFObject] -> IO (Fallible ([PDFObject], [ByteString], Int, Int))
runPass disabled objects = evalPDFWorkT $ do
  importObjects (fromList objects)
  modify' $ \work -> work { wSettings = (wSettings work)
    { sCompressor = UseDeflate, sOptimizeGFX = if disabled then DoNotOptimizeGFX else sOptimizeGFX (wSettings work) } }
  modifyIndirectObjectsP (filterOptimize GfxOptimization)
  before <- gets (size . wPDF)
  repeatedFormFragments
  after <- gets (size . wPDF)
  pdf <- gets wPDF
  streams <- mapM (unfilter >=> getStream) (IM.elems (ppObjectsWithStream pdf))
  pure (IM.elems (ppObjectsWithoutStream pdf) ++ IM.elems (ppObjectsWithStream pdf), streams, before, after)
 where
  size :: PDFPartition -> Int
  size pdf = sum (map (BS.length . fromPDFObject) (IM.elems (ppObjectsWithoutStream pdf) ++ IM.elems (ppObjectsWithStream pdf)))

forms :: [PDFObject] -> [PDFObject]
forms = filter ((== Just (PDFName "Form")) . getValueForKey "Subtype")

expectSkipped :: [PDFObject] -> IO ()
expectSkipped objects = do
  result <- runPass False objects
  case result of
    Right (output, _, before, after) -> do
      forms output `shouldBe` forms objects
      after `shouldBe` before
    Left err -> fail (show err)

spec :: Spec
spec = describe "repeated form fragments" $ do
  it "shares repeated geometry across pages only when compressed total cost decreases" $ do
    result <- runPass False document
    case result of
      Right (output, streams, before, after) -> do
        length (forms output) `shouldBe` 1
        after `shouldSatisfy` (< before)
        length (filter (BS.isInfixOf "Do") streams) `shouldBe` 4
        map (getValueForKey "Resources") (forms output) `shouldBe` [Just (mkPDFDictionary [])]
      Left err -> fail (show err)

  it "respects disabled graphics optimization" $ do
    result <- runPass True document
    case result of
      Right (output, _, before, after) -> do
        forms output `shouldBe` []
        after `shouldBe` before
      Left err -> fail (show err)

  it "rejects cheap and unique fragments" $ do
    expectSkipped (concat [page n "q 0 g 0 0 1 1 re f Q" [] | n <- [1,3,5,7]])
    expectSkipped (page 1 ("q " <> fragment <> "Q") [])
    expectSkipped (concat [page n ("q 0 g " <> BS.concat (replicate 100 "0 0 1 1 re f ") <> "Q") [] | n <- [1,3]])

  it "replans offsets and resource names after committing another group" $ do
    let other = "0.6 g " <> BS.concat [BC.pack (unwords [show ((i*i*31) `mod` 997), show ((i*i*53) `mod` 991), "0.123 0.456 re f "]) | i <- [1 :: Int .. 250]]
        input = concat [page n ("q " <> fragment <> "Q q 0.5 0 0 0.5 0 0 cm " <> fragment <> "Q q " <> other <> "Q") [] | n <- [1,3,5,7]]
    result <- runPass False input
    case result of
      Right (output, streams, before, after) -> do
        length (forms output) `shouldBe` 2
        after `shouldSatisfy` (< before)
        length (filter (BS.isInfixOf "/RF1 Do") streams) `shouldBe` 4
      Left err -> fail (show err)

  it "does not extract inherited colours, strokes, clipping or internal matrices" $ do
    mapM_ (\body -> expectSkipped (concat [page n ("q " <> body <> " Q") [] | n <- [1,3,5,7]]))
      [BS.drop 15 fragment, fragment <> "0 0 2 2 re S", fragment <> "0 0 2 2 re W n", fragment <> "1 0 0 1 0 0 cm"]

  it "keeps leading CTMs at call sites and shares the same form" $ do
    let input = concat [page n ("q 1 0 0 1 " <> BC.pack (show n) <> " 0 cm " <> fragment <> "Q") [] | n <- [1,3,5,7]]
    result <- runPass False input
    case result of
      Right (output, streams, _, _) -> do
        length (forms output) `shouldBe` 1
        length (filter (BS.isInfixOf "cm") streams) `shouldBe` 4
      Left err -> fail (show err)

  it "remaps image resources by identity and avoids parent name collisions" $ do
    let resources name = mkPDFDictionary [("XObject", mkPDFDictionary [(name, PDFReference 20 0), ("RF0", PDFReference 21 0)])]
        input = concat [page n ("q " <> fragment <> "/" <> name <> " Do Q") [("Resources", resources name)] | (n, name) <- [(1,"A"),(3,"B"),(5,"C"),(7,"D")]]
        image n = PDFIndirectObjectWithStream n 0 (Map.fromList [("Subtype", PDFName "Image"), ("ColorSpace", PDFName "DeviceGray"), ("Width", PDFNumber 1), ("Height", PDFNumber 1), ("BitsPerComponent", PDFNumber 8)]) "x"
    result <- runPass False (input ++ [image 20, image 21])
    case result of
      Right (output, streams, _, _) -> do
        length (forms output) `shouldBe` 1
        map (getValueForKey "Resources") (forms output) `shouldBe`
          [Just (mkPDFDictionary [("XObject", mkPDFDictionary [("I0", PDFReference 20 0)])])]
        length (filter (BS.isInfixOf "/RF1 Do") streams) `shouldBe` 4
      Left err -> fail (show err)

  it "resolves inherited resources and indirect resource categories" $ do
    let input = concat [page n ("q " <> fragment <> "Q") [("Parent", PDFReference 20 0)] | n <- [1,3,5,7]]
        parent = PDFIndirectObject 20 0 (mkPDFDictionary [("Resources", PDFReference 21 0)])
        resources = PDFIndirectObject 21 0 (mkPDFDictionary [("XObject", PDFReference 22 0)])
        xs = PDFIndirectObject 22 0 (mkPDFDictionary [])
    result <- runPass False (input ++ [parent, resources, xs])
    case result of
      Right (output, _, _, _) -> length (forms output) `shouldBe` 1
      Left err -> fail (show err)

  it "bounds negative rectangles and Bezier control points without page-space clipping" $ do
    let geometry :: ByteString
        geometry = "6000 8000 -8000 -12000 re f 0 0 m 7000 9000 0 0 1 1 c f "
        input = concat [page n ("q " <> fragment <> geometry <> "Q") [] | n <- [1,3,5,7]]
    result <- runPass False input
    case result of
      Right (output, _, _, _) -> map (getValueForKey "BBox") (forms output) `shouldBe`
        [Just (mkPDFArray (map PDFNumber [-2001, -4001, 7001, 9001]))]
      Left err -> fail (show err)

  it "does not confuse identical resource names that refer to different images" $ do
    let input = concat [page n ("q " <> fragment <> "/X Do Q")
          [("Resources", mkPDFDictionary [("XObject", mkPDFDictionary [("X", PDFReference (n + 20) 0)])])]
          | n <- [1,3,5,7]]
        images = [PDFIndirectObjectWithStream (n + 20) 0 (Map.fromList
          [("Subtype", PDFName "Image"), ("ColorSpace", PDFName "DeviceGray")]) "x" | n <- [1,3,5,7]]
    expectSkipped (input ++ images)

  it "rejects transparency, tagging, inline images and unknown operators" $ do
    mapM_ (\prefix -> expectSkipped (concat [page n (prefix <> " q " <> fragment <> " Q") [] | n <- [1,3,5,7]]))
      ["/Alpha gs", "/Span BMC EMC", "/Span << /MCID 0 >> BDC EMC", "1 Tr", "unknown", "BI /W 1 /H 1 /BPC 8 /CS /G ID x EI"]
    expectSkipped (document ++ [PDFIndirectObject 20 0 (mkPDFDictionary [("StructTreeRoot", PDFReference 21 0)])])
    expectSkipped (concat [page n ("q " <> fragment <> "Q") [("Group", mkPDFDictionary [("S", PDFName "Transparency")])] | n <- [1,3,5,7]])

  it "rejects inherited paths, unterminated paths, text and unbalanced saves" $ do
    mapM_ (\(prefix, suffix) -> expectSkipped (concat [page n (prefix <> fragment <> suffix) [] | n <- [1,3,5,7]]))
      [("0 0 m q ", "Q"), ("q ", "0 0 m Q"), ("BT q ", "Q ET"), ("q ", "Q Q"), ("q ", "")]

  it "rejects default colour-space substitutions and cyclic inheritance" $ do
    expectSkipped (concat [page n ("q " <> fragment <> "Q") [("Resources", mkPDFDictionary [("ColorSpace", mkPDFDictionary [("DefaultRGB", PDFName "DeviceRGB")])])] | n <- [1,3,5,7]])
    expectSkipped (concat [page n ("q " <> fragment <> "Q") [("Parent", PDFReference n 0)] | n <- [1,3,5,7]])

  it "does not mutate content streams shared with another owner" $ do
    expectSkipped (document ++ [PDFIndirectObject 20 0 (mkPDFDictionary [("Other", PDFReference 2 0), ("Also", PDFReference 4 0), ("More", PDFReference 6 0)])])

  it "rejects nested Form XObjects and images with soft masks" $ do
    let input = concat [page n ("q " <> fragment <> "/X Do Q") [("Resources", mkPDFDictionary [("XObject", mkPDFDictionary [("X", PDFReference 20 0)])])] | n <- [1,3,5,7]]
    expectSkipped (input ++ [PDFIndirectObjectWithStream 20 0 (Map.singleton "Subtype" (PDFName "Form")) ""])
    expectSkipped (input ++ [PDFIndirectObjectWithStream 20 0 (Map.fromList [("Subtype", PDFName "Image"), ("ColorSpace", PDFName "DeviceRGB"), ("SMask", PDFReference 21 0)]) ""])
