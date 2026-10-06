module PDF.Document.LimitImageResolutionSpec (spec) where

import Codec.Compression.Flate qualified as Flate
import Control.Monad.State (gets, modify)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Fallible (Fallible)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.PDF.PDFDocument (fromList)
import Data.PDF.PDFObject
  (PDFObject (PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNumber, PDFReference, PDFTrailer, PDFVersion), mkPDFArray, mkPDFDictionary)
import Data.PDF.PDFPartition (ppObjectsWithStream)
import Data.PDF.PDFWork (PDFWork, evalPDFWorkT, getObject)
import Data.PDF.Settings (sLimitDPI, sCompressor, UseCompressor (UseDeflate))
import Data.PDF.WorkData (wPDF, wSettings, wBitmaps)
import Data.Bitmap.Bitmap (bitmapOriginalEncoding, bitmapSamples, BitmapEncoding (JPEGEncoding))
import PDF.Document.LimitImageResolution (imagePixelLimits, limitImageResolution)
import PDF.Document.Encode (pdfEncode)
import External.ExternalCommand (externalCommandBuf)
import External.ImageMagick (jpegQuality)
import PDF.Processing.Optimize qualified as Optimize
import PDF.Object.Object.Properties (getValueForKey)
import PDF.Object.Object.FromPDFObject (fromPDFObject)
import PDF.Object.State (getStream)
import PDF.Processing.PDFWork (importObjects)
import PDF.Processing.Unfilter (unfilter)
import Test.Hspec (Spec, Expectation, describe, it, shouldBe, pendingWith, expectationFailure)
import System.Directory.Extra (findExecutable)
import Util.Dictionary (mkDictionary)

box :: [Double] -> PDFObject
box = mkPDFArray . map PDFNumber

resources :: PDFObject
resources = mkPDFDictionary [("XObject", mkPDFDictionary
  [("Im", PDFReference 10 0), ("Form", PDFReference 20 0)])]

page :: Int -> Int -> [(ByteString, PDFObject)] -> PDFObject
page number contents extras = PDFIndirectObject number 0 $ mkPDFDictionary
  ([("Type", PDFName "Page"), ("MediaBox", box [0, 0, 612, 792]),
    ("Resources", resources), ("Contents", PDFReference contents 0)] ++ extras)

content :: Int -> ByteString -> PDFObject
content number = PDFIndirectObjectWithStream number 0 mempty

image :: Int -> Int -> Int -> PDFObject -> ByteString -> PDFObject
image width height bits colorSpace = PDFIndirectObjectWithStream 10 0 (mkDictionary
  [("Subtype", PDFName "Image"), ("Width", PDFNumber (fromIntegral width)),
    ("Height", PDFNumber (fromIntegral height)), ("BitsPerComponent", PDFNumber (fromIntegral bits)),
    ("ColorSpace", colorSpace)])

gray :: PDFObject
gray = image 144 144 8 (PDFName "DeviceGray") (BS.replicate (144 * 144) 120)

run :: [PDFObject] -> PDFWork IO a -> IO (Fallible a)
run objects action = evalPDFWorkT $ importObjects (fromList objects) >> action

limits :: Double -> [PDFObject] -> IO (Fallible (Maybe (IntMap (Int, Int))))
limits dpi objects = run objects (imagePixelLimits dpi)

resize :: Maybe Double -> [PDFObject] -> IO (Fallible (Maybe PDFObject))
resize dpi objects = run objects $ do
  modify (\state -> state {wSettings = (wSettings state) {sLimitDPI = dpi}})
  limitImageResolution
  getObject 10

-- PDFObject equality checks indirect identity, so compare the actual content.
shouldKeep :: Fallible (Maybe PDFObject) -> PDFObject -> Expectation
shouldKeep result original =
  fmap (fmap fromPDFObject) result `shouldBe` Right (Just (fromPDFObject original))

spec :: Spec
spec = describe "Image DPI limit" $ do
  it "uses page-space points and physical page size" $ do
    result <- limits 100 [page 1 2 [], content 2 "612 0 0 792 0 0 cm /Im Do", gray]
    result `shouldBe` Right (Just (IM.singleton 10 (850, 1100)))
    small <- limits 100 [page 1 2 [], content 2 "72 0 0 36 0 0 cm /Im Do", gray]
    small `shouldBe` Right (Just (IM.singleton 10 (100, 50)))
  it "accounts for UserUnit, inherited page boxes and non-zero origins" $ do
    let parent = PDFIndirectObject 3 0 (mkPDFDictionary
          [("MediaBox", box [10, 20, 622, 812]), ("CropBox", box [10, 20, 310, 400]),
           ("Resources", resources)])
        inheritedPage = PDFIndirectObject 1 0 (mkPDFDictionary
          [("Type", PDFName "Page"), ("Parent", PDFReference 3 0),
           ("UserUnit", PDFNumber 2), ("Contents", PDFReference 2 0)])
    result <- limits 100 [inheritedPage, parent, content 2 "72 0 0 36 10 20 cm /Im Do", gray]
    result `shouldBe` Right (Just (IM.singleton 10 (200, 100)))
  it "does not shrink the full image according to clipping or cropping" $ do
    result <- limits 100 [page 1 2 [("CropBox", box [0, 0, 36, 36])],
      content 2 "0 0 18 18 re W n 72 0 0 72 0 0 cm /Im Do", gray]
    result `shouldBe` Right (Just (IM.singleton 10 (100, 100)))
  it "takes the maximum dimensions over every page and repeated invocation" $ do
    result <- limits 72 [page 1 2 [], content 2 "q 36 0 0 72 0 0 cm /Im Do Q 72 0 0 36 0 0 cm /Im Do",
      page 3 4 [], content 4 "108 0 0 18 0 0 cm /Im Do", gray]
    result `shouldBe` Right (Just (IM.singleton 10 (108, 72)))
  it "uses image axes for rotated and reflected placements" $ do
    result <- limits 100 [page 1 2 [("Rotate", PDFNumber 90)],
      content 2 "0 72 -36 0 100 100 cm /Im Do", gray]
    result `shouldBe` Right (Just (IM.singleton 10 (100, 50)))
  it "recurses into Forms with their Matrix and resource scope" $ do
    let form = PDFIndirectObjectWithStream 20 0 (mkDictionary
          [("Subtype", PDFName "Form"), ("Matrix", box [2, 0, 0, 3, 0, 0]),
           ("Resources", mkPDFDictionary [("XObject", mkPDFDictionary [("Nested", PDFReference 10 0)])])])
          "q 36 0 0 12 0 0 cm /Nested Do Q"
    result <- limits 100 [page 1 2 [], content 2 "q /Form Do Q /Im Do", gray, form]
    result `shouldBe` Right (Just (IM.singleton 10 (100, 50)))
  it "interprets content arrays as one program" $ do
    let arrayPage = PDFIndirectObject 1 0 (mkPDFDictionary
          [("Type", PDFName "Page"), ("MediaBox", box [0, 0, 612, 792]),
           ("Resources", resources), ("Contents", mkPDFArray [PDFReference 2 0, PDFReference 3 0])])
    result <- limits 100 [arrayPage, content 2 "q 72 0 0 36 0 0 cm", content 3 "/Im Do Q", gray]
    result `shouldBe` Right (Just (IM.singleton 10 (100, 50)))
  it "retains images when a placement, page box or recursive Form is uncertain" $ do
    bad <- limits 100 [page 1 2 [], content 2 "q /Im Do", gray]
    bad `shouldBe` Right Nothing
    unit <- limits 100 [page 1 2 [("UserUnit", PDFNumber 0)], content 2 "/Im Do", gray]
    unit `shouldBe` Right Nothing
    cyclic <- limits 100 [page 1 2 [], content 2 "/Form Do", gray,
      PDFIndirectObjectWithStream 20 0 (mkDictionary [("Subtype", PDFName "Form")]) "/Form Do"]
    cyclic `shouldBe` Right Nothing
  it "downsamples eligible images once and updates their dictionaries" $ do
    result <- resize (Just 72) [page 1 2 [], content 2 "72 0 0 36 0 0 cm /Im Do", gray]
    case result of
      Right (Just output) -> do
        getValueForKey "Width" output `shouldBe` Just (PDFNumber 72)
        getValueForKey "Height" output `shouldBe` Just (PDFNumber 36)
        getValueForKey "BitsPerComponent" output `shouldBe` Just (PDFNumber 8)
        getValueForKey "Filter" output `shouldBe` Just (PDFName "FlateDecode")
        bytes <- run [output] (unfilter output >>= getStream)
        bytes `shouldBe` Right (BS.replicate (72 * 36) 120)
      _ -> result `shouldBe` Right (Just gray)
  it "is disabled by default and never enlarges images" $ do
    disabled <- resize Nothing [page 1 2 [], content 2 "/Im Do", gray]
    shouldKeep disabled gray
    unchanged <- resize (Just 300) [page 1 2 [], content 2 "72 0 0 72 0 0 cm /Im Do", gray]
    shouldKeep unchanged gray
  it "excludes packed-bit and Indexed images" $ do
    let fourBit = image 144 144 4 (PDFName "DeviceGray") (BS.replicate (144 * 72) 0)
        indexed = image 144 144 8 (mkPDFArray [PDFName "Indexed", PDFName "DeviceGray", PDFNumber 0, PDFNumber 0]) (BS.replicate (144 * 144) 0)
    packed <- resize (Just 72) [page 1 2 [], content 2 "/Im Do", fourBit]
    shouldKeep packed fourBit
    palette <- resize (Just 72) [page 1 2 [], content 2 "/Im Do", indexed]
    shouldKeep palette indexed
  it "preserves 16-bit component values" $ do
    let sixteenBit = image 2 1 16 (PDFName "DeviceGray") (BS.pack [0, 0, 255, 255])
    result <- run [page 1 2 [], content 2 "/Im Do", sixteenBit] $ do
      modify (\state -> state {wSettings = (wSettings state) {sLimitDPI = Just 72}})
      limitImageResolution
      streams <- gets (ppObjectsWithStream . wPDF)
      case IM.lookup 10 streams of
        Just output -> unfilter output >>= getStream
        Nothing -> return BS.empty
    result `shouldBe` Right (BS.pack [128, 0])
  it "decodes Flate streams and removes stale predictor parameters" $ do
    let compressed = PDFIndirectObjectWithStream 10 0 (mkDictionary
          [("Subtype", PDFName "Image"), ("Width", PDFNumber 2), ("Height", PDFNumber 1),
           ("BitsPerComponent", PDFNumber 8), ("ColorSpace", PDFName "DeviceGray"),
           ("Filter", PDFName "FlateDecode"), ("DecodeParms", mkPDFDictionary
             [("Predictor", PDFNumber 12), ("Columns", PDFNumber 2)])])
          (either (error . show) id (Flate.fastCompress (BS.pack [2, 0, 200])))
    result <- resize (Just 72) [page 1 2 [], content 2 "/Im Do", compressed]
    case result of
      Right (Just output) -> do
        getValueForKey "DecodeParms" output `shouldBe` Nothing
        bytes <- run [output] (unfilter output >>= getStream)
        bytes `shouldBe` Right (BS.pack [100])
      _ -> result `shouldBe` Right (Just compressed)
  it "protects images also used in annotation appearances" $ do
    let appearance = PDFIndirectObject 3 0 (mkPDFDictionary [("AP", mkPDFDictionary [("N", PDFReference 20 0)])])
        form = PDFIndirectObjectWithStream 20 0 (mkDictionary [("Subtype", PDFName "Form"),
          ("Resources", resources)]) "/Im Do"
    result <- resize (Just 72) [page 1 2 [], content 2 "/Im Do", gray, appearance, form]
    shouldKeep result gray
  it "resolves color-space aliases in page resources and excludes Indexed aliases" $ do
    let aliases = mkPDFDictionary [("XObject", mkPDFDictionary [("Im", PDFReference 10 0)]),
          ("ColorSpace", mkPDFDictionary [("CS", PDFName "DeviceRGB")])]
        rgb = image 2 1 8 (PDFName "CS") (BS.pack [0, 20, 40, 100, 120, 140])
        indexedAliases = mkPDFDictionary [("XObject", mkPDFDictionary [("Im", PDFReference 10 0)]),
          ("ColorSpace", mkPDFDictionary [("CS", mkPDFArray [PDFName "Indexed", PDFName "DeviceRGB", PDFNumber 0, PDFNumber 0])])]
    result <- resize (Just 72) [page 1 2 [("Resources", aliases)], content 2 "/Im Do", rgb]
    case result of
      Right (Just output) -> do
        getValueForKey "Width" output `shouldBe` Just (PDFNumber 1)
        bytes <- run [output] (unfilter output >>= getStream)
        bytes `shouldBe` Right (BS.pack [50, 70, 90])
      _ -> expectationFailure (show result)
    indexed <- resize (Just 72) [page 1 2 [("Resources", indexedAliases)], content 2 "/Im Do", rgb]
    shouldKeep indexed rgb
  it "retains shared images with conflicting color-space scopes" $ do
    let scoped color = mkPDFDictionary [("XObject", mkPDFDictionary [("Im", PDFReference 10 0)]),
          ("ColorSpace", mkPDFDictionary [("CS", color)])]
        shared = image 2 1 8 (PDFName "CS") (BS.pack [0, 200])
    result <- resize (Just 72) [page 1 2 [("Resources", scoped (PDFName "DeviceGray"))],
      page 3 4 [("Resources", scoped (PDFName "DeviceRGB"))], content 2 "/Im Do", content 4 "/Im Do", shared]
    shouldKeep result shared
  it "decodes JPEG images before downsampling" $ do
    executable <- findExecutable "convert"
    case executable of
      Nothing -> pendingWith "ImageMagick convert is not installed"
      Just _ -> do
        encoded <- runExceptT (externalCommandBuf "convert" ["-size", "4x2", "xc:red", "jpg:-"] BS.empty)
        case encoded of
          Left err -> expectationFailure (show err)
          Right jpeg -> do
            let object = PDFIndirectObjectWithStream 10 0 (mkDictionary
                  [("Subtype", PDFName "Image"), ("Width", PDFNumber 4), ("Height", PDFNumber 2),
                   ("BitsPerComponent", PDFNumber 8), ("ColorSpace", PDFName "DeviceRGB"),
                   ("Filter", PDFName "DCTDecode")]) jpeg
            result <- resize (Just 72) [page 1 2 [], content 2 "/Im Do", object]
            case result of
              Right (Just output) -> do
                getValueForKey "Width" output `shouldBe` Just (PDFNumber 1)
                getValueForKey "Height" output `shouldBe` Just (PDFNumber 1)
                bytes <- run [output] (unfilter output >>= getStream)
                fmap BS.length bytes `shouldBe` Right 3
              _ -> expectationFailure (show result)
  forM_ [25, 80] $ \quality ->
    it ("preserves source JPEG quality " ++ show quality ++ " through resizing and optimization") $ do
      executable <- findExecutable "convert"
      case executable of
        Nothing -> pendingWith "ImageMagick convert is not installed"
        Just _ -> do
          encoded <- runExceptT (externalCommandBuf "convert"
            ["-size", "64x32", "gradient:red-blue", "-quality", show quality, "jpg:-"] BS.empty)
          case encoded of
            Left err -> expectationFailure (show err)
            Right jpeg -> do
              let wrapped = either (error . show) id (Flate.fastCompress jpeg)
                  object = PDFIndirectObjectWithStream 10 0 (mkDictionary
                    [("Subtype", PDFName "Image"), ("Width", PDFNumber 64), ("Height", PDFNumber 32),
                     ("BitsPerComponent", PDFNumber 8), ("ColorSpace", PDFName "DeviceRGB"),
                     ("Filter", mkPDFArray [PDFName "FlateDecode", PDFName "DCTDecode"])]) wrapped
              result <- run [page 1 2 [], content 2 "32 0 0 16 0 0 cm /Im Do", object] $ do
                modify (\state -> state {wSettings = (wSettings state) {sLimitDPI = Just 72, sCompressor = UseDeflate}})
                limitImageResolution
                bitmaps <- gets wBitmaps
                resized <- getObject 10
                optimized <- maybe (return object) (Optimize.optimize Nothing) resized
                decoded <- unfilter optimized
                bytes <- getStream decoded
                return (fmap bitmapOriginalEncoding (IM.lookup 10 bitmaps),
                  fmap (BS.length . bitmapSamples) (IM.lookup 10 bitmaps),
                  getValueForKey "Width" optimized, getValueForKey "Height" optimized, bytes)
              case result of
                Left err -> expectationFailure (show err)
                Right (hint, lengthOfSamples, width, height, bytes) -> do
                  hint `shouldBe` Just (JPEGEncoding quality)
                  lengthOfSamples `shouldBe` Just (32 * 16 * 3)
                  width `shouldBe` Just (PDFNumber 32)
                  height `shouldBe` Just (PDFNumber 16)
                  BS.take 2 bytes `shouldBe` "\xff\xd8"
                  actualQuality <- runExceptT (jpegQuality bytes)
                  actualQuality `shouldBe` Right quality
  it "limits resolution after pruning and before bit-depth optimization in the encoder" $ do
    let catalog = PDFIndirectObject 100 0 (mkPDFDictionary
          [("Type", PDFName "Catalog"), ("Pages", PDFReference 101 0)])
        pages = PDFIndirectObject 101 0 (mkPDFDictionary
          [("Type", PDFName "Pages"), ("Kids", mkPDFArray [PDFReference 1 0]), ("Count", PDFNumber 1)])
        document = fromList [PDFVersion "1.7", catalog, pages,
          page 1 2 [("Parent", PDFReference 101 0)], content 2 "72 0 0 36 0 0 cm /Im Do", gray,
          PDFIndirectObjectWithStream 11 0 (mkDictionary [("Subtype", PDFName "Image")]) "unused",
          PDFTrailer (mkPDFDictionary [("Root", PDFReference 100 0)])]
    result <- evalPDFWorkT $ do
      modify (\state -> state {wSettings = (wSettings state) {sLimitDPI = Just 72, sCompressor = UseDeflate}})
      _ <- pdfEncode document
      output <- getObject 10
      unused <- getObject 11
      return (fmap (getValueForKey "Width") output, fmap (getValueForKey "Height") output,
        fmap (getValueForKey "BitsPerComponent") output, unused)
    result `shouldBe` Right (Just (Just (PDFNumber 72)), Just (Just (PDFNumber 36)),
      Just (Just (PDFNumber 1)), Nothing)
