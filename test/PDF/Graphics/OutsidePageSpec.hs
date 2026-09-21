module PDF.Graphics.OutsidePageSpec (spec) where

import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.PDF.Program (Program, parseProgram)
import PDF.Graphics.InvisibleImages (Rect (Rect))
import PDF.Graphics.OutsidePage (FontInfo (FontInfo), removeOutsidePage)
import PDF.Graphics.Parser.Stream (gfxParse)
import Test.Hspec (Spec, describe, it, shouldBe)

parse :: ByteString -> Program
parse bytes = either (error . show) parseProgram (gfxParse bytes)

optimize :: ByteString -> Program
optimize =
  removeOutsidePage
    (Rect 0 0 100 100)
    ( Map.singleton
        "F"
        ( FontInfo
            (Rect (-200) (-200) 1200 1000)
            (Map.fromList [(i, 600) | i <- [0 .. 255]])
        )
    )
    (Map.fromList [("Form", Rect 200 200 220 220), ("Im", Rect 0 0 1 1)])
    . parse

spec :: Spec
spec = describe "Outside page painting" $ do
  it "removes fills on each side, retaining intersecting and touching bounds" $ do
    optimize "110 10 5 5 re f -20 10 5 5 re f 10 -20 5 5 re f 10 110 5 5 re f"
      `shouldBe` parse ""
    let visible :: ByteString
        visible = "95 10 10 10 re f 100 10 5 5 re f"
    optimize visible `shouldBe` parse visible
  it "bounds all subpaths and Bezier control points" $ do
    optimize "110 110 m 120 140 130 150 140 110 c h f" `shouldBe` parse ""
    let visible :: ByteString
        visible = "110 110 m 50 50 130 150 140 110 c f 110 110 5 5 re 10 10 5 5 re f"
    optimize visible `shouldBe` parse visible
  it "applies transforms when points are constructed and restores q/Q" $ do
    optimize "q 0 1 -1 0 200 0 cm 0 0 10 10 re f Q 0 0 10 10 re f"
      `shouldBe` parse "q 0 1 -1 0 200 0 cm Q 0 0 10 10 re f"
    optimize "110 110 m q 1 0 0 1 -200 -200 cm 0 0 l Q f"
      `shouldBe` parse "110 110 m q 1 0 0 1 -200 -200 cm 0 0 l Q f"
  it "allows state changes between discarded path commands" $ do
    optimize "110 110 m 1 0 0 rg 120 120 l f 0 0 5 5 re f"
      `shouldBe` parse "1 0 0 rg 0 0 5 5 re f"
  it "includes stroke width, joins, caps and transformed stroke expansion" $ do
    optimize "200 200 m 220 220 l S" `shouldBe` parse ""
    let visible :: ByteString
        visible = "20 w 105 5 m 105 50 l S 0 w 200 200 m 210 210 l S"
    optimize visible `shouldBe` parse visible
    let scaled :: ByteString
        scaled = "110 10 m 110 20 l 100 0 0 100 0 0 cm S"
    optimize scaled `shouldBe` parse scaled
  it "preserves clipping paths while replacing their off-page painting with n" $ do
    optimize "200 200 10 10 re W B 0 0 10 10 re f"
      `shouldBe` parse "200 200 10 10 re W n 0 0 10 10 re f"
  it "keeps uncertain strokes after ExtGState but still removes fills" $ do
    optimize "/GS gs 200 200 10 10 re S 200 200 10 10 re f"
      `shouldBe` parse "/GS gs 200 200 10 10 re S"
  it "removes bounded XObjects and transformed images" $ do
    optimize "/Form Do q 1 0 0 1 200 0 cm /Im Do Q /Unknown Do"
      `shouldBe` parse "q 1 0 0 1 200 0 cm Q /Unknown Do"
  it "removes off-page text before ET without changing text parameters" $ do
    optimize "BT /F 10 Tf 1 0 0 1 200 20 Tm (abc) Tj ET"
      `shouldBe` parse "BT /F 10 Tf 1 0 0 1 200 20 Tm ET"
  it "uses glyph bounds rather than the text origin" $ do
    let visible :: ByteString
        visible = "BT /F 10 Tf 1 0 0 1 101 20 Tm (a) Tj ET"
    optimize visible `shouldBe` parse visible
  it "retains advances needed by subsequent visible text" $ do
    let visible :: ByteString
        visible = "BT /F 10 Tf 1 0 0 1 -20 20 Tm (abc) Tj (def) Tj ET"
    optimize visible `shouldBe` parse visible
    optimize "BT /F 10 Tf 1 0 0 1 200 20 Tm (abc) Tj 1 0 0 1 10 20 Tm (def) Tj ET"
      `shouldBe` parse "BT /F 10 Tf 1 0 0 1 200 20 Tm 1 0 0 1 10 20 Tm (def) Tj ET"
  it "handles TJ adjustments, hex strings, spacing, rise and horizontal scaling" $ do
    let visible :: ByteString
        visible = "BT /F 10 Tf 1 0 0 1 200 20 Tm [<61> 20000 (b)] TJ ET"
    optimize visible `shouldBe` parse visible
    optimize "BT /F 10 Tf 1000 Ts 2 Tc 3 Tw -100 Tz [(a b) 20 <63>] TJ ET"
      `shouldBe` parse "BT /F 10 Tf 1000 Ts 2 Tc 3 Tw -100 Tz ET"
  it "preserves text clipping and fonts without trustworthy metrics" $ do
    let clipping :: ByteString
        clipping = "BT /F 10 Tf 7 Tr 1 0 0 1 200 20 Tm (abc) Tj ET"
        unknown :: ByteString
        unknown = "BT /Missing 10 Tf 1 0 0 1 200 20 Tm (abc) Tj ET"
    optimize clipping `shouldBe` parse clipping
    optimize unknown `shouldBe` parse unknown
  it "preserves quote side effects and line positioning" $ do
    optimize
      "BT /F 10 Tf 1 0 0 1 200 20 Tm (abc) Tj 2 3 (def) \" 1 0 0 1 10 20 Tm (x) Tj ET"
      `shouldBe` parse "BT /F 10 Tf 1 0 0 1 200 20 Tm 2 3 (def) \" 1 0 0 1 10 20 Tm (x) Tj ET"
  it "leaves unknown syntax and malformed nesting unchanged" $ do
    let unknown :: ByteString
        unknown = "200 200 10 10 re f unknown"
        unbalanced :: ByteString
        unbalanced = "q 200 200 10 10 re f"
    optimize unknown `shouldBe` parse unknown
    optimize unbalanced `shouldBe` parse unbalanced
  it "uses the CTM and text matrix together for rotated text" $ do
    optimize "q 0 1 -1 0 200 0 cm BT /F 10 Tf (abc) Tj ET Q"
      `shouldBe` parse "q 0 1 -1 0 200 0 cm BT /F 10 Tf ET Q"
  it "keeps stroked glyphs whose outline reaches the page" $ do
    let stroked :: ByteString
        stroked = "20 w BT /F 10 Tf 1 Tr 1 0 0 1 110 20 Tm (abc) Tj ET"
    optimize stroked `shouldBe` parse stroked
  it "preserves line movement after deleting an off-page text suffix" $ do
    optimize "BT /F 10 Tf 1 0 0 1 200 20 Tm (abc) Tj -180 0 Td (abc) Tj ET"
      `shouldBe` parse "BT /F 10 Tf 1 0 0 1 200 20 Tm -180 0 Td (abc) Tj ET"
  it "restores font metrics after leaving a saved graphics state" $ do
    optimize
      "BT /F 10 Tf 1 0 0 1 200 20 Tm q /Missing 10 Tf (abc) Tj Q 1 0 0 1 200 20 Tm (abc) Tj ET"
      `shouldBe` parse
        "BT /F 10 Tf 1 0 0 1 200 20 Tm q /Missing 10 Tf (abc) Tj Q 1 0 0 1 200 20 Tm ET"
  it "removes inline images with off-page unit-square bounds" $ do
    optimize "q 1 0 0 1 200 0 cm BI /W 1 /H 1 /BPC 8 /CS /G ID x EI Q"
      `shouldBe` parse "q 1 0 0 1 200 0 cm Q"
  it "preserves malformed marked content" $ do
    let malformed :: ByteString
        malformed = "/P BMC 200 200 10 10 re f"
    optimize malformed `shouldBe` parse malformed
