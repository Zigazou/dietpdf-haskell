module PDF.Graphics.Interpreter.RenameResourcesSpec (spec) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.PDF.Program (parseProgram)
import Data.PDF.Resource (Resource (ResExtGState, ResColorSpace, ResPattern, ResFont, ResXObject, ResShading, ResProperties))
import PDF.Graphics.Interpreter.RenameResources (renameResources)
import PDF.Graphics.Parser.Stream (gfxParse)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "graphics resource renaming" $ do
  let table = Map.fromList
        [(category "R9", category name) | (category, name) <-
          [(ResExtGState, "state"), (ResColorSpace, "space"),
           (ResPattern, "pattern"), (ResFont, "font"),
           (ResXObject, "object"), (ResShading, "shading"),
           (ResProperties, "properties")]]
      examples :: [(ByteString, ByteString)]
      examples =
        [("/R9 cs /R9 scn", "/space cs /pattern scn"),
         ("/R9 CS /R9 SCN", "/space CS /pattern SCN"),
         ("/R9 gs /R9 Do /R9 12 Tf /R9 sh",
          "/state gs /object Do /font 12 Tf /shading sh"),
         ("/Tag /R9 BDC /Tag /R9 DP", "/Tag /properties BDC /Tag /properties DP"),
         (".2 .4 .6 /R9 scn .5 /R9 SCN",
          ".2 .4 .6 /pattern scn .5 /pattern SCN"),
         ("/Unknown cs /Unknown scn", "/Unknown cs /Unknown scn")]
  forM_ examples $ \(input, expected) ->
    it ("uses the operator's resource category for " ++ show input) $
      (renameResources table . parseProgram <$> gfxParse input)
        `shouldBe` (parseProgram <$> gfxParse expected)
  it "does not use an ExtGState translation for an unknown color space" $
    (renameResources (Map.singleton (ResExtGState "R9") (ResExtGState "state"))
      . parseProgram <$> gfxParse "/R9 cs")
      `shouldBe` (parseProgram <$> gfxParse "/R9 cs")
