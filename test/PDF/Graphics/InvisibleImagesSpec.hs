module PDF.Graphics.InvisibleImagesSpec (spec) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject (GFXObject (GFXName), GSOperator (GSPaintXObject))
import Data.PDF.Program (parseProgram)
import Data.Foldable (toList)
import PDF.Graphics.InvisibleImages (Rect (Rect), ImageInfo (ImageInfo), removeInvisibleImages)
import PDF.Graphics.Parser.Stream (gfxParse)
import Test.Hspec (Spec, describe, it, shouldBe)

remaining :: ByteString -> [ByteString]
remaining bytes = case gfxParse bytes of
  Left err -> error (show err)
  Right tokens ->
    [name | Command GSPaintXObject parameters <- toList
      (removeInvisibleImages (Rect 0 0 100 100)
        (Map.fromList [("Im",ImageInfo True),("Mask",ImageInfo False)]) (parseProgram tokens)),
      GFXName name <- toList parameters]

spec :: Spec
spec = describe "Invisible image analysis" $ do
  it "removes images beyond the page and keeps partially visible images" $ do
    remaining "q 10 0 0 10 110 0 cm /Im Do Q" `shouldBe` []
    remaining "q 10 0 0 10 95 0 cm /Im Do Q" `shouldBe` ["Im"]
  it "composes transforms in PDF order and restores saved state" $ do
    remaining "q 10 0 0 10 0 0 cm 1 0 0 1 11 0 cm /Im Do Q /Im Do" `shouldBe` ["Im"]
  it "handles rotated and reflected image bounds conservatively" $ do
    remaining "q 0 10 -10 0 120 0 cm /Im Do Q" `shouldBe` []
    remaining "q -10 0 0 10 5 0 cm /Im Do Q" `shouldBe` ["Im"]
  it "removes images covered by a subsequent opaque rectangle" $ do
    remaining "/Im Do -1 -1 3 3 re f" `shouldBe` []
  it "combines several opaque covers" $ do
    remaining "/Im Do -1 -1 1.6 3 re f .4 -1 2 3 re f" `shouldBe` []
  it "does not mistake earlier paint, partial coverage or strokes for occlusion" $ do
    remaining "0 0 1 1 re f /Im Do" `shouldBe` ["Im"]
    remaining "/Im Do 0 0 .5 1 re f" `shouldBe` ["Im"]
    remaining "/Im Do 0 0 1 1 re S" `shouldBe` ["Im"]
  it "uses subsequent opaque images but never masked images as covers" $ do
    remaining "/Im Do /Im Do" `shouldBe` ["Im"]
    remaining "/Im Do /Mask Do" `shouldBe` ["Im","Mask"]
    remaining "/Mask Do /Im Do" `shouldBe` ["Im"]
  it "applies clipping at path termination and restores it with Q" $ do
    remaining "q 10 10 10 10 re W n /Im Do Q /Im Do" `shouldBe` ["Im"]
    remaining "10 10 10 10 re W /Im Do n" `shouldBe` ["Im"]
  it "does not use complex or clipped fills as proof of full coverage" $ do
    remaining "/Im Do 0 0 .5 1 re W n 0 0 1 1 re f" `shouldBe` ["Im"]
    remaining "/Im Do 0 0 1 1 re .2 .2 .5 .5 re f*" `shouldBe` ["Im"]
    remaining "/Im Do 0 0 m 1 1 l W n 0 0 1 1 re f" `shouldBe` ["Im"]
  it "keeps the current path outside the q/Q stack" $ do
    remaining "/Im Do q -1 -1 3 3 re Q f" `shouldBe` []
  it "retains uncertain transparency, forms, patterns and optional content" $ do
    remaining "/Im Do /GS gs 0 0 1 1 re f" `shouldBe` ["Im"]
    remaining "/Im Do /Form Do 0 0 1 1 re f" `shouldBe` ["Im","Form"]
    remaining "/Im Do /Pattern cs /P scn 0 0 1 1 re f" `shouldBe` ["Im"]
    remaining "/Im Do /OC /Layer BDC 0 0 1 1 re f EMC" `shouldBe` ["Im"]
  it "keeps unknown syntax and unbalanced graphics stacks unchanged" $ do
    remaining "110 0 0 110 110 110 cm /Im Do unknown" `shouldBe` ["Im"]
    remaining "q 1 0 0 1 110 0 cm /Im Do" `shouldBe` ["Im"]
    remaining "1 0 0 1 110 0 cm /Im Do Q" `shouldBe` ["Im"]
  it "does not bridge a tiny gap in opaque coverage" $ do
    remaining "/Im Do 0 0 .499999999 1 re f .5 0 .5 1 re f" `shouldBe` ["Im"]

  it "preserves images at antialiased fill boundaries" $ do
    remaining "/Im Do 0 0 1 1 re f" `shouldBe` ["Im"]
    remaining "/Im Do -1 -1 1.5 3 re f .5 -1 2 3 re f" `shouldBe` ["Im"]
  it "handles text without treating text clipping as an opaque cover" $ do
    remaining "BT ET /Im Do -1 -1 3 3 re f" `shouldBe` []
    remaining "BT 7 Tr ET /Im Do -1 -1 3 3 re f" `shouldBe` ["Im"]

  it "accepts nested structural marked content independently of q/Q" $ do
    remaining "/P<</MCID 0>>BDC /Figure BMC q 1 0 0 1 110 0 cm /Im Do EMC Q EMC" `shouldBe` []
    remaining "/Figure /Props BDC /Im Do EMC -1 -1 3 3 re f" `shouldBe` []
    remaining "/Figure<</MCID 1>>BDC /Im Do EMC" `shouldBe` ["Im"]
  it "retains optional content and malformed marked-content nesting" $ do
    remaining "/Im Do /OC /Layer BDC q Q -1 -1 3 3 re f EMC" `shouldBe` ["Im"]
    remaining "q 1 0 0 1 110 0 cm /Im Do Q EMC" `shouldBe` ["Im"]
    remaining "/Figure BMC q 1 0 0 1 110 0 cm /Im Do Q" `shouldBe` ["Im"]
