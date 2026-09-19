module PDF.Processing.DuplicatedObjectsSpec (spec) where

import Control.Monad (forM_)
import Data.Fallible (Fallible)
import Util.Dictionary (Dictionary)
import Data.ByteString (ByteString)
import Data.IntMap.Strict qualified as IntMap
import Data.Map qualified as Map
import Data.PDF.PDFObject (PDFObject (PDFIndirectObjectWithStream, PDFName, PDFNumber, PDFReference, PDFDictionary), mkPDFArray)
import Data.PDF.PDFPartition (PDFPartition (PDFPartition))
import Data.PDF.PDFWork (evalPDFWorkT)
import PDF.Processing.DuplicatedObjects (duplicateCount, findDuplicatedObjects)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "findDuplicatedObjects" $ do
  let dictionary :: Dictionary PDFObject
      dictionary = Map.fromList
        [ ("Subtype", PDFName "Image")
        , ("ColorSpace", PDFName "DeviceGray")
        , ("BitsPerComponent", PDFNumber 8)
        , ("Width", PDFNumber 2)
        , ("Height", PDFNumber 1)
        ]
      count :: Dictionary PDFObject -> Dictionary PDFObject -> IO (Fallible Int)
      count first second = do
        let objects = IntMap.fromList
              [ (5, PDFIndirectObjectWithStream 5 0 first "\0\0")
              , (6, PDFIndirectObjectWithStream 6 0 second "\0\0")
              ]
        result <- evalPDFWorkT $ findDuplicatedObjects
          (PDFPartition objects mempty mempty mempty)
        return (duplicateCount <$> result)

  it "merges matching streams and dictionaries" $
    count dictionary dictionary >>= (`shouldBe` Right 1)

  it "ignores the encoded stream length" $
    count (Map.insert "Length" (PDFNumber 2) dictionary)
          (Map.insert "Length" (PDFNumber 12) dictionary)
      >>= (`shouldBe` Right 1)

  it "keeps a title image distinct from its soft mask" $
    count dictionary
      (Map.insert "SMask" (PDFReference 5 0)
        $ Map.insert "ColorSpace" (PDFReference 185 0) dictionary)
      >>= (`shouldBe` Right 0)

  forM_ ([ ("SMask", PDFReference 5 0)
         , ("ColorSpace", PDFName "DeviceRGB")
         , ("Width", PDFNumber 1)
         , ("Height", PDFNumber 2)
         , ("BitsPerComponent", PDFNumber 1)
         , ("Decode", mkPDFArray [PDFNumber 1, PDFNumber 0])
         , ("Filter", PDFName "DCTDecode")
         , ("Resources", PDFDictionary $ Map.singleton "Font" (PDFReference 20 0))
         ] :: [(ByteString, PDFObject)]) $ \(key, value) ->
    it ("preserves differences in " ++ show key) $
      count dictionary (Map.insert key value dictionary)
        >>= (`shouldBe` Right 0)
