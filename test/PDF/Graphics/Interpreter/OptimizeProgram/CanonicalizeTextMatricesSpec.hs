module PDF.Graphics.Interpreter.OptimizeProgram.CanonicalizeTextMatricesSpec (spec) where

import Control.Monad (forM_, replicateM)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Kind (Type)
import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IM
import Data.PDF.Command (Command (cOperator, cParameters), mkCommand)
import Data.PDF.GFXObject (GFXObject (GFXString, GFXArray, GFXNumber), GSOperator (GSBeginText, GSSetTextMatrix, GSMoveToNextLine, GSMoveToNextLineLP,
  GSNextLine, GSSetTextLeading, GSSetCharacterSpacing, GSSetWordSpacing,
  GSSetHorizontalScaling, GSSetTextFont, GSShowText, GSShowManyText, GSNLShowText))
import Data.PDF.PDFObject (PDFObject (PDFName, PDFNumber), mkPDFArray, mkPDFDictionary)
import Data.PDF.Program (Program, parseProgram, programComputedSize)
import Data.PDF.WorkData (emptyWorkData)
import Data.Ratio ((%))
import Data.Sequence qualified as SQ
import PDF.Graphics.Interpreter.OptimizeProgram (optimizeProgramWithTextResources)
import PDF.Graphics.Interpreter.OptimizeProgram.CanonicalizeTextMatrices (canonicalizeTextMatrices)
import PDF.Graphics.Parser.Stream (gfxParse)
import PDF.Graphics.TextMetrics (TextResources, buildTextResources)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (Gen, choose, elements, forAll, property)
import Util.Dictionary (mkDictionary)

parse :: ByteString -> Program
parse input = either (error . show) parseProgram (gfxParse input)

simpleFont :: Double -> PDFObject
simpleFont width = mkPDFDictionary
  [("Subtype",PDFName "TrueType"),("FirstChar",PDFNumber 32),("LastChar",PDFNumber 66),
   ("Widths",mkPDFArray (map PDFNumber (250 : replicate 32 width ++ [width,600])))]

resources :: TextResources
resources = buildTextResources IM.empty (Just (mkDictionary
  [("Font",mkPDFDictionary [("F",simpleFont 500),("G",simpleFont 700),
     ("CID",mkPDFDictionary [("Subtype",PDFName "Type0"),("Encoding",PDFName "Identity-H"),
       ("DescendantFonts",mkPDFArray [mkPDFDictionary [("Subtype",PDFName "CIDFontType2"),
         ("DW",PDFNumber 500)]])])]),
   ("ExtGState",mkPDFDictionary [("Change",mkPDFDictionary
     [("Font",mkPDFArray [simpleFont 700,PDFNumber 10])]),("Keep",mkPDFDictionary [])])]))

optimize :: ByteString -> Program
optimize = canonicalizeTextMatrices resources False . parse

spec :: Spec
spec = describe "CanonicalizeTextMatrices" $ do
  forM_
    [ ("BT /F 10 Tf 1 0 0 1 10 20 Tm (A) Tj 1 0 0 1 15 20 Tm (B) Tj ET",
       "BT /F 10 Tf 10 20 Td (A) Tj (B) Tj ET")
    , ("BT /F 10 Tf 10 20 Td [(A) 500] TJ 0 0 Td (B) Tj ET",
       "BT /F 10 Tf 10 20 Td [(A) 500] TJ (B) Tj ET")
    , ("BT /F 10 Tf 10 20 Td (A) Tj 1 0 0 1 10 20 Tm (B) Tj ET",
       "BT /F 10 Tf 10 20 Td (A) Tj T* (B) Tj ET")
    , ("BT /F 10 Tf 10 20 Td (A) Tj 1 0 0 1 15 20 Tm (B) Tj 0 -12 Td (A) Tj ET",
       "BT /F 10 Tf 10 20 Td (A) Tj 5 0 Td (B) Tj 0 -12 Td (A) Tj ET")
    , ("BT /F 10 Tf 2 Tc 3 Tw 50 Tz 10 20 Td (A ) Tj 1 0 0 1 17.25 20 Tm (B) Tj ET",
       "BT /F 10 Tf 2 Tc 3 Tw 50 Tz 10 20 Td (A ) Tj (B) Tj ET")
    , ("BT /CID 10 Tf 20 Tw 10 20 Td <0020> Tj 1 0 0 1 15 20 Tm (A) Tj ET",
       "BT /CID 10 Tf 20 Tw 10 20 Td <0020> Tj (A) Tj ET")
    , ("BT /F 10 Tf 0 1 -1 0 10 20 Tm (A) Tj 0 1 -1 0 10 25 Tm (B) Tj ET",
       "BT /F 10 Tf 0 1 -1 0 10 20 Tm (A) Tj (B) Tj ET")
    , ("BT /F 10 Tf 1 .5 .25 1 10 20 Tm (A) Tj 1 .5 .25 1 15 22.5 Tm (B) Tj ET",
       "BT /F 10 Tf 1 .5 .25 1 10 20 Tm (A) Tj (B) Tj ET")
    , ("BT /F 10 Tf 12 TL 10 20 Td (A) ' 1 0 0 1 15 8 Tm (B) Tj ET",
       "BT /F 10 Tf 12 TL 10 20 Td (A) ' (B) Tj ET")
    , ("BT /F 10 Tf 12 TL 10 20 Td 3 2 (A ) \" 1 0 0 1 24.5 8 Tm (B) Tj ET",
       "BT /F 10 Tf 12 TL 10 20 Td 3 2 (A ) \" (B) Tj ET")
    , ("BT /F 10 Tf 10 20 Td /Change gs (A) Tj 1 0 0 1 17 20 Tm (B) Tj ET",
       "BT /F 10 Tf 10 20 Td /Change gs (A) Tj (B) Tj ET")
    , ("BT /F 10 Tf 10 20 Td /Keep gs (A) Tj 1 0 0 1 15 20 Tm (B) Tj ET",
       "BT /F 10 Tf 10 20 Td /Keep gs (A) Tj (B) Tj ET")
    , ("BT /F 10 Tf 10 20 Td /G 10 Tf (A) Tj 1 0 0 1 17 20 Tm (B) Tj ET",
       "BT /F 10 Tf 10 20 Td /G 10 Tf (A) Tj (B) Tj ET")
    , ("BT /F 0 Tf 10 20 Td (A) Tj 0 0 Td (B) Tj ET",
       "BT /F 0 Tf 10 20 Td (A) Tj (B) Tj ET")
    ] $ \(input, expected) -> it (show input) $
      optimize input `shouldBe` parse expected

  it "preserves leading when TD becomes shorter than Tm or when only TL is needed" $ do
    let input :: ByteString
        input = "BT /F 10 Tf 10 -12 TD (A) Tj 1 0 0 1 10 -24 Tm (B) Tj T* (A) Tj ET"
    optimize input `shouldBe` parse "BT /F 10 Tf 10 -12 TD (A) Tj T* (B) Tj T* (A) Tj ET"
    optimize "BT 0 0 0 0 10 20 Tm 0 -12 TD (A) Tj T* (B) Tj ET"
      `shouldBe` parse "BT 0 0 0 0 10 20 Tm 12 TL (A) Tj T* (B) Tj ET"

  it "does not infer glyph advances with ambiguous resources or unknown gs fonts" $ do
    let input = parse "BT /F 10 Tf 10 20 Td (A) Tj 1 0 0 1 15 20 Tm (B) Tj ET"
        unknown = buildTextResources IM.empty Nothing
    canonicalizeTextMatrices unknown False input
      `shouldBe` parse "BT /F 10 Tf 10 20 Td (A) Tj 5 0 Td (B) Tj ET"
    optimize "BT /F 10 Tf 10 20 Td /Unknown gs (A) Tj 1 0 0 1 15 20 Tm (B) Tj ET"
      `shouldBe` parse "BT /F 10 Tf 10 20 Td /Unknown gs (A) Tj 5 0 Td (B) Tj ET"

  it "keeps inherited Form spacing unknown until explicitly established" $ do
    let input = parse "BT /F 10 Tf 10 20 Td (A) Tj 1 0 0 1 15 20 Tm (B) Tj ET"
    canonicalizeTextMatrices resources True input
      `shouldBe` parse "BT /F 10 Tf 10 20 Td (A) Tj 5 0 Td (B) Tj ET"
    canonicalizeTextMatrices resources True
      (parse "BT /F 10 Tf 0 Tc 100 Tz 10 20 Td (A) Tj 1 0 0 1 15 20 Tm (B) Tj ET")
      `shouldBe` parse "BT /F 10 Tf 0 Tc 100 Tz 10 20 Td (A) Tj (B) Tj ET"

  it "does not remove a reset after q / Q inside a text object" $
    optimize "BT /F 10 Tf 10 20 Td q (A) Tj Q 1 0 0 1 15 20 Tm (B) Tj ET"
      `shouldBe` parse "BT /F 10 Tf 10 20 Td q (A) Tj Q 1 0 0 1 15 20 Tm (B) Tj ET"

  it "does not interpret malformed Tj operands as TJ adjustments" $
    optimize "BT /F 10 Tf 10 20 Td 500 Tj 1 0 0 1 5 20 Tm (B) Tj ET"
      `shouldBe` parse "BT /F 10 Tf 10 20 Td 500 Tj 1 0 0 1 5 20 Tm (B) Tj ET"

  it "restores font parameters across q/Q outside text objects" $
    optimize "BT /F 10 Tf ET q BT /G 10 Tf (A) Tj ET Q BT 10 20 Td (A) Tj 1 0 0 1 15 20 Tm (B) Tj ET"
      `shouldBe` parse "BT /F 10 Tf ET q BT /G 10 Tf (A) Tj ET Q BT 10 20 Td (A) Tj (B) Tj ET"

  it "counts advances in invisible text and ignores rise in the displacement" $
    optimize "BT /F 10 Tf 3 Tr 2 Ts 10 20 Td (A) Tj 1 0 0 1 15 20 Tm (B) Tj ET"
      `shouldBe` parse "BT /F 10 Tf 3 Tr 2 Ts 10 20 Td (A) Tj (B) Tj ET"

  it "rejects non-serializable inverses instead of introducing coordinate drift" $
    optimize "BT /F 10 Tf 3 0 0 1 10 20 Tm (A) Tj 3 0 0 1 11 20 Tm (B) Tj ET"
      `shouldBe` parse "BT /F 10 Tf 3 0 0 1 10 20 Tm (A) Tj 3 0 0 1 11 20 Tm (B) Tj ET"

  it "integrates with the full program optimizer" $ do
    let input = parse "BT /F 10 Tf 10 20 Td (A) Tj 1 0 0 1 15 20 Tm (B) Tj ET"
        output = optimizeProgramWithTextResources resources False emptyWorkData input
    trace output `shouldBe` trace input
    programComputedSize output `shouldSatisfy` (< programComputedSize input)

  it "preserves inherited leading through the full optimization pipeline" $
    optimizeProgramWithTextResources resources True emptyWorkData
      (parse "BT 0 0 Td T* (A) Tj ET")
      `shouldBe` parse "BT T* (A) Tj ET"

  it "does not fold relative moves using a matrix assumed across Q" $ do
    let input = parse "BT /F 10 Tf 10 20 Td q 0 0 0 0 100 200 Tm (B) Tj Q 3 4 Td 5 6 Td (A) Tj ET"
    optimizeProgramWithTextResources resources False emptyWorkData input `shouldBe` input

  it "preserves explicit font resets after ExtGState font selection" $ do
    let output = optimizeProgramWithTextResources resources False emptyWorkData
          (parse "BT /F 10 Tf /Change gs /F 10 Tf 10 20 Td (A) Tj 1 0 0 1 15 20 Tm (B) Tj ET")
    length (filter ((== GSSetTextFont) . cOperator) (toList output)) `shouldBe` 2

  it "does not confuse a 1 percent Tz operand with a unit scaling factor" $ do
    let input = parse "BT /F 10 Tf 1 Tz 10 20 Td (A) Tj 1 0 0 1 10.05 20 Tm (B) Tj ET"
    trace (optimizeProgramWithTextResources resources False emptyWorkData input)
      `shouldBe` trace input

  it "preserves glyph positions through the complete optimization pipeline" $
    property $ forAll genProgram $ \input ->
      trace (optimizeProgramWithTextResources resources False emptyWorkData input) == trace input

  it "preserves every glyph position against an independent rational interpreter" $
    property $ forAll genProgram $ \input ->
      let output = canonicalizeTextMatrices resources False input
      in trace output == trace input && programComputedSize output <= programComputedSize input

-- Independent reference interpreter: integer input operands, rational glyph
-- widths and explicit component-wise translation (no production matrix helpers).
type Matrix :: Type
type Matrix = (Rational, Rational, Rational, Rational, Rational, Rational)
type ReferenceState :: Type
type ReferenceState = (Matrix, Matrix, Rational, Rational, Rational, Rational, Rational, [Matrix])

trace :: Program -> [Matrix]
trace program = reverse marks
 where
  (_, _, _, _, _, _, _, marks) = foldl step (ident, ident, 0, 0, 0, 1, 10, []) (toList program)
  ident :: Matrix
  ident = (1,0,0,1,0,0)
  decimal :: Double -> Rational
  decimal n = round (n * 1000000) % 1000000
  nums :: Command -> [Rational]
  nums cmd = [decimal n | GFXNumber n <- toList (cParameters cmd)]
  translate :: Matrix -> Rational -> Rational -> Matrix
  translate (a,b,c,d,e,f) x y = (a,b,c,d,e + a * x + c * y,f + b * x + d * y)
  step :: ReferenceState -> Command -> ReferenceState
  step state@(tm,lm,tl,tc,tw,hz,fs,seen) cmd = case (cOperator cmd, nums cmd, toList (cParameters cmd)) of
    (GSBeginText,_,_) -> (ident,ident,tl,tc,tw,hz,fs,seen)
    (GSSetTextMatrix,[a,b,c,d,e,f],_) -> let m = (a,b,c,d,e,f) in (m,m,tl,tc,tw,hz,fs,seen)
    (GSMoveToNextLine,[x,y],_) -> let m = translate lm x y in (m,m,tl,tc,tw,hz,fs,seen)
    (GSMoveToNextLineLP,[x,y],_) -> let m = translate lm x y in (m,m,-y,tc,tw,hz,fs,seen)
    (GSNextLine,_,_) -> let m = translate lm 0 (-tl) in (m,m,tl,tc,tw,hz,fs,seen)
    (GSSetTextLeading,[l],_) -> (tm,lm,l,tc,tw,hz,fs,seen)
    (GSSetCharacterSpacing,[v],_) -> (tm,lm,tl,v,tw,hz,fs,seen)
    (GSSetWordSpacing,[v],_) -> (tm,lm,tl,tc,v,hz,fs,seen)
    (GSSetHorizontalScaling,[v],_) -> (tm,lm,tl,tc,tw,v / 100,fs,seen)
    (GSSetTextFont,[v],_) -> (tm,lm,tl,tc,tw,hz,v,seen)
    (GSShowText,_,[GFXString bytes]) -> display state (BSC.unpack bytes)
    (GSShowManyText,_,[GFXArray items]) -> foldl item state (toList items)
    (GSNLShowText,_,[GFXString bytes]) -> display (step state (mkCommand GSNextLine [])) (BSC.unpack bytes)
    _ -> state
   where
    display :: ReferenceState -> String -> ReferenceState
    display st bytes = foldl glyph st bytes
    glyph :: ReferenceState -> Char -> ReferenceState
    glyph (m,l,lLead,c,w,h,s,positions) ch =
      let width = if ch == 'A' then 500 else if ch == 'B' then 600 else 250
          dx = (width * s / 1000 + c + if ch == ' ' then w else 0)*h
      in (translate m dx 0,l,lLead,c,w,h,s,m:positions)
    item :: ReferenceState -> GFXObject -> ReferenceState
    item (m,l,lLead,c,w,h,s,positions) (GFXNumber v) = (translate m (-decimal v * s * h / 1000) 0,l,lLead,c,w,h,s,positions)
    item st (GFXString bytes) = display st (BSC.unpack bytes)
    item st _ = st

genProgram :: Gen Program
genProgram = do
  count <- choose (1,40)
  body <- replicateM count $ do
    op <- elements [0 :: Int .. 10]
    x <- choose (-20,20 :: Int)
    y <- choose (-20,20 :: Int)
    a <- elements [0,1,2,-1,3]
    b <- elements [0,1,-1]
    text <- elements ["A","B","A B","AA"]
    let ns :: [Int] -> [GFXObject]
        ns = map (GFXNumber . fromIntegral)
    pure $ case op of
      0 -> mkCommand GSSetTextMatrix (ns [a,b,0,1,x,y])
      1 -> mkCommand GSMoveToNextLine (ns [x,y])
      2 -> mkCommand GSMoveToNextLineLP (ns [x,y])
      3 -> mkCommand GSNextLine []
      4 -> mkCommand GSSetTextLeading (ns [y])
      5 -> mkCommand GSSetCharacterSpacing (ns [x])
      6 -> mkCommand GSSetWordSpacing (ns [x])
      7 -> mkCommand GSSetHorizontalScaling (ns [50 + x])
      8 -> mkCommand GSShowManyText [GFXArray (SQ.fromList [GFXString text,GFXNumber (fromIntegral x * 100)])]
      9 -> mkCommand GSNLShowText [GFXString text]
      _ -> mkCommand GSShowText [GFXString text]
  pure (parse "BT /F 10 Tf" <> SQ.fromList body <> parse "ET")
