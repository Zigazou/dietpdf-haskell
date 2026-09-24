module PDF.Graphics.Interpreter.OptimizeProgram.OptimizeClipPathsSpec (spec) where

import Control.Monad (forM_)
import Data.Map.Strict qualified as Map
import Data.PDF.Command (Command, mkCommand)
import Data.PDF.GFXObject (
  GFXObject (GFXDictionary, GFXInlineImage, GFXName, GFXNumber, GFXString),
  GSOperator (
    GSBeginInlineImage,
    GSBeginMarkedContentSequence,
    GSBeginMarkedContentSequencePL,
    GSBeginText,
    GSEndMarkedContentSequence,
    GSEndPath,
    GSEndText,
    GSFillPathEOR,
    GSFillPathNZWR,
    GSIntersectClippingPathEOR,
    GSIntersectClippingPathNZWR,
    GSLineTo,
    GSMarkedContentPointPL,
    GSMoveTo,
    GSPaintShapeColourShading,
    GSPaintXObject,
    GSRectangle,
    GSRestoreGS,
    GSSaveGS,
    GSSetCTM,
    GSSetLineWidth,
    GSSetNonStrokeColorN,
    GSShowText,
    GSStrokePath,
    GSUnknown
  ),
 )
import Data.PDF.Program (mkProgram)
import Data.PDF.WorkData (emptyWorkData)
import PDF.Graphics.Interpreter.OptimizeProgram (optimizeProgram)
import PDF.Graphics.Interpreter.OptimizeProgram.OptimizeClipPaths (
  optimizeClipPaths,
 )
import Test.Hspec (Spec, describe, it, shouldBe)

rect :: [Double] -> Command
rect = mkCommand GSRectangle . map GFXNumber

spec :: Spec
spec = describe "optimizeClipPaths" $ do
  forM_ [GSIntersectClippingPathNZWR, GSIntersectClippingPathEOR] $ \rule -> do
    let w = mkCommand rule []
        n = mkCommand GSEndPath []
        s = mkCommand GSStrokePath []
        q = mkCommand GSSaveGS []
        restore = mkCommand GSRestoreGS []
        small = rect [0, 0, 100, 100]
        large = rect [-50, -50, 300, 300]
        clip r = [r, w, n]
        base = clip small
        check input expected = optimizeClipPaths (mkProgram input) `shouldBe` mkProgram expected
        cm = mkCommand GSSetCTM . map GFXNumber
    it "keeps the first clip and removes an enclosing clip" $
      check (base ++ clip large) base
    it "keeps painting when its clipping operator is redundant" $
      check (base ++ [large, w, s]) (base ++ [large, s])
    it "keeps disjoint clips, then removes redundant clips in the empty region" $ do
      let empty = base ++ clip (rect [200, 200, 50, 50])
      check (empty ++ clip large) empty
    it "intersects overlapping rectangles" $ do
      let overlap = base ++ clip (rect [50, 50, 100, 100])
      check (overlap ++ clip (rect [40, 40, 70, 70])) overlap
    it "restores clipping bounds at Q" $
      check
        ([q] ++ base ++ [restore] ++ clip large)
        ([q] ++ base ++ [restore] ++ clip large)
    it "restores the CTM at Q" $
      check
        (base ++ [q, cm [1, 0, 0, 1, 1000, 0], restore] ++ clip large)
        (base ++ [q, cm [1, 0, 0, 1, 1000, 0], restore])
    it "accounts for translations" $
      check
        (base ++ [cm [1, 0, 0, 1, 1000, 0]] ++ clip large)
        (base ++ [cm [1, 0, 0, 1, 1000, 0]] ++ clip large)
    it "supports negative rectangle dimensions" $
      check (base ++ clip (rect [150, 150, -200, -200])) base
    it "preserves skewed paths instead of comparing their bounding boxes" $
      check
        (base ++ [cm [1, 1, 0, 1, 0, 0]] ++ clip large)
        (base ++ [cm [1, 1, 0, 1, 0, 0]] ++ clip large)
    it "preserves compound paths and even-odd holes" $
      check (base ++ [large, small, w, n]) (base ++ [large, small, w, n])
    it "does not save the current path at q" $
      check
        (base ++ [mkCommand GSMoveTo [GFXNumber 0, GFXNumber 0], q] ++ clip large)
        (base ++ [mkCommand GSMoveTo [GFXNumber 0, GFXNumber 0], q] ++ clip large)
    it "does not apply unterminated clips" $
      check (base ++ [large, w]) (base ++ [large, w])
    it "composes transformations in PDF order" $ do
      let first = cm [2, 0, 0, 2, 0, 0]
          second = cm [1, 0, 0, 1, 10, 10]
          prefix = [first] ++ base ++ [second]
      check (prefix ++ clip (rect [-10, -10, 100, 100])) prefix
    it "defers clipping until the path terminator" $
      check
        ([small, w, q, n, restore] ++ clip large)
        ([small, w, q, n, restore] ++ clip large)
    it "works inside the complete optimization pipeline" $
      optimizeProgram emptyWorkData (mkProgram (base ++ clip large ++ [small, s]))
        `shouldBe` optimizeProgram emptyWorkData (mkProgram (base ++ [small, s]))
    it "is idempotent" $ do
      let result = optimizeClipPaths (mkProgram (base ++ clip large ++ clip large))
      optimizeClipPaths result `shouldBe` result

    let empty = base ++ clip (rect [200, 200, 50, 50])
        fill = mkCommand GSFillPathNZWR []
        image = mkCommand GSBeginInlineImage [GFXInlineImage mempty "pixels"]
        shading = mkCommand GSPaintShapeColourShading [GFXName "Sh1"]
        form = mkCommand GSPaintXObject [GFXName "Fm1"]
        text =
          [ mkCommand GSBeginText []
          , mkCommand GSShowText [GFXString "searchable"]
          , mkCommand GSEndText []
          ]
        mark =
          mkCommand
            GSBeginMarkedContentSequencePL
            [GFXName "Figure", GFXDictionary (Map.singleton "MCID" (GFXNumber 1))]
        endMark = mkCommand GSEndMarkedContentSequence []
        drawing = [small, s, image, shading]
    it "removes invisible paths, inline images and shadings under an empty clip" $
      check (empty ++ drawing) empty
    it "preserves text and unresolved forms under an empty clip" $
      check (empty ++ text ++ [form] ++ drawing) (empty ++ text ++ [form])
    it "preserves every operation in MCID marked content" $
      check
        (empty ++ [mark] ++ drawing ++ text ++ [endMark] ++ drawing)
        (empty ++ [mark] ++ drawing ++ text ++ [endMark])
    it "protects nested marked content independently of q/Q" $ do
      let marked =
            [mark, q, mkCommand GSBeginMarkedContentSequence [GFXName "Artifact"]]
              ++ drawing
              ++ [endMark, restore]
              ++ drawing
              ++ [endMark]
      check (empty ++ marked ++ drawing) (empty ++ marked)
    it "preserves paths crossing a marked-content boundary" $
      check (empty ++ [mark, small, endMark, s]) (empty ++ [mark, small, endMark, s])
    it "preserves paths containing a marked-content point" $ do
      let point = mkCommand GSMarkedContentPointPL [GFXName "Figure", GFXDictionary mempty]
      check (empty ++ [small, point, s]) (empty ++ [small, point, s])
    it "preserves drawing inside a text object" $ do
      let protected = [mkCommand GSBeginText []] ++ drawing ++ [mkCommand GSEndText []]
      check (empty ++ protected) (empty ++ protected)
    it "keeps state changes needed after restoring the clip" $ do
      let width = mkCommand GSSetLineWidth [GFXNumber 5]
      check
        ([q] ++ empty ++ [small, width, s, restore, small, s])
        ([q] ++ empty ++ [width, restore, small, s])
    it "removes arbitrary invisible paths without dropping interleaved state" $ do
      let move = mkCommand GSMoveTo [GFXNumber 0, GFXNumber 0]
          line = mkCommand GSLineTo [GFXNumber 10, GFXNumber 10]
          width = mkCommand GSSetLineWidth [GFXNumber 5]
      check (empty ++ [move, width, line, s]) (empty ++ [width])
    it "keeps the path and clip effect when replacing invisible painting with n" $ do
      let r = rect [200, 200, 50, 50]
      check (base ++ [r, w, fill] ++ drawing) (base ++ [r, w, n])
    it "does not use a pending clip to remove its own stroke" $ do
      let r = rect [200, 200, 50, 50]
      check (base ++ [r, w, s] ++ drawing) (base ++ [r, w, s])
    it "removes an invisible arbitrary clipping path's painting but keeps its clip" $ do
      let move = mkCommand GSMoveTo [GFXNumber 0, GFXNumber 0]
          line = mkCommand GSLineTo [GFXNumber 10, GFXNumber 10]
      check (empty ++ [move, line, w, s]) (empty ++ [move, line, w, n])
    it "removes disjoint rectangle fills but preserves strokes and touching fills" $
      forM_ [GSFillPathNZWR, GSFillPathEOR] $ \fillOp -> do
        let f = mkCommand fillOp []
            outsideRect = rect [200, 200, 50, 50]
            touching = rect [100, 0, 50, 50]
        check
          (base ++ [outsideRect, f, outsideRect, s, touching, f])
          (base ++ [outsideRect, s, touching, f])
    it "removes inline images outside a nonempty clip" $ do
      let translation = cm [1, 0, 0, 1, 200, 200]
      check (base ++ [translation, image]) (base ++ [translation])
    it "retains visible and partially clipped inline images" $
      check
        (base ++ [image, cm [100, 0, 0, 100, 50, 50], image])
        (base ++ [image, cm [100, 0, 0, 100, 50, 50], image])
    it "preserves unterminated paths even under an empty clip" $
      check (empty ++ [small]) (empty ++ [small])
    it "leaves malformed marked content and unknown operators unchanged" $ do
      let malformed = empty ++ [endMark] ++ drawing
          unknown = empty ++ [mkCommand (GSUnknown "extension") []] ++ drawing
      check malformed malformed
      check unknown unknown
    it "keeps invisible marked content through the complete optimization pipeline" $
      optimizeProgram
        emptyWorkData
        (mkProgram (empty ++ [mark] ++ drawing ++ [endMark] ++ drawing))
        `shouldBe` optimizeProgram
          emptyWorkData
          (mkProgram (empty ++ [mark] ++ drawing ++ [endMark]))
    it "is idempotent after removing invisible graphics" $ do
      let result = optimizeClipPaths (mkProgram (empty ++ drawing ++ text))
      optimizeClipPaths result `shouldBe` result

    it "preserves pattern painting that could contain text or marked content" $ do
      let patternColor = mkCommand GSSetNonStrokeColorN [GFXName "P1"]
      check (empty ++ [patternColor, small, fill]) (empty ++ [patternColor, small, fill])
