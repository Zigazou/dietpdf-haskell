module PDF.Graphics.ImageBoundsSpec (spec) where

import Data.ByteString (ByteString)
import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject (GFXObject (GFXNumber), GSOperator (GSSetCTM))
import Data.PDF.Program (mkProgram, parseProgram)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import PDF.Graphics.Geometry (Rect (Rect))
import PDF.Graphics.ImageBounds (boundingSize, imageBounds, imageSize)
import PDF.Graphics.Parser.Stream (gfxParse)
import Test.Hspec (Spec, describe, it, shouldBe)

analyze :: ByteString -> Maybe [(ByteString, Rect)]
analyze bytes = case gfxParse bytes of
  Left err -> error (show err)
  Right tokens -> imageBounds (Set.fromList ["Im", "Mask"]) (parseProgram tokens)

spec :: Spec
spec = describe "Bitmap XObject bounds" $ do
  it "returns all occurrences in order and filters non-bitmap names" $ do
    analyze "/Im Do /Form Do /Mask Do /Im Do"
      `shouldBe` Just [("Im", Rect 0 0 1 1), ("Mask", Rect 0 0 1 1), ("Im", Rect 0 0 1 1)]
    analyze "" `shouldBe` Just []
  it "composes matrices in PDF order and restores nested graphics states" $
    analyze "10 0 0 20 5 7 cm q 1 0 0 1 2 3 cm /Im Do q 2 0 0 2 0 0 cm /Im Do Q /Im Do Q /Im Do"
      `shouldBe` Just [("Im", Rect 25 67 35 87), ("Im", Rect 25 67 45 107), ("Im", Rect 25 67 35 87), ("Im", Rect 5 7 15 27)]
  it "handles rotation, reflection, shear and singular matrices" $ do
    analyze "q 0 10 -20 0 5 7 cm /Im Do Q"
      `shouldBe` Just [("Im", Rect (-15) 7 5 17)]
    analyze "q -10 0 0 -20 5 7 cm /Im Do Q"
      `shouldBe` Just [("Im", Rect (-5) (-13) 5 7)]
    analyze "q 2 3 -4 5 10 20 cm /Im Do Q"
      `shouldBe` Just [("Im", Rect 6 20 12 28)]
    analyze "0 0 0 0 5 7 cm /Im Do"
      `shouldBe` Just [("Im", Rect 5 7 5 7)]
  it "reports full geometry independently of clipping and text matrices" $
    analyze "0 0 .5 .5 re W n BT 100 0 0 100 20 30 Tm ET /Im Do"
      `shouldBe` Just [("Im", Rect 0 0 1 1)]
  it "rejects malformed commands and unbalanced saves or restores" $
    map analyze ["q /Im Do", "/Im Do Q", "1 q", "q 1 Q", "1 2 cm /Im Do", "1 Do", "/Im /Im Do", "unknown /Im Do"]
      `shouldBe` replicate 8 Nothing
  it "rejects non-finite matrix operands" $
    map (imageBounds Set.empty . mkProgram . pure . Command GSSetCTM . Seq.fromList . map GFXNumber)
      [[1, 0, 0, 1, 0 / 0, 0], [1, 0, 0, 1, 1 / 0, 0]]
      `shouldBe` [Nothing, Nothing]
  it "provides width and height in user-space units" $
    boundingSize (Rect (-15) 7 5 17) `shouldBe` (20, 10)
  it "measures image axes independently of axis-aligned bounding rectangles" $
    imageSize (0, 72, -36, 0, 100, 100) `shouldBe` (72, 36)
