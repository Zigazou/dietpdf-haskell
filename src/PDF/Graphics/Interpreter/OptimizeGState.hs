{-|
Factorization of graphics state parameters into reusable resources

Optimizes PDF graphics programs by factorizing sequences of graphics state
setting commands into reusable external graphics state (ExtGState) resources.

Multiple consecutive graphics state commands (line width, line cap, line join,
miter limit, dash pattern, color rendering intent, flatness) can be
replaced with a single @GSSetParameters@ command referencing a named ExtGState
resource. This reduces the size of graphics streams when the same state
combinations are used multiple times or can be shared across the PDF.

The module identifies contiguous sequences of factorizable commands and replaces
them with parameterized resource references.
-}
module PDF.Graphics.Interpreter.OptimizeGState
  ( optimizeGState
  , planGState
  , gStateCost
  )
where

import Control.Monad.State (get, put)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.Logging (Logging)
import Data.PDF.Command (Command (Command), mkCommand)
import Data.PDF.ExtGState (mkExtGState)
import Data.PDF.GFXObject
  ( GFXObject (GFXName, GFXNumber, GFXArray)
  , separateGfx
  , GSOperator (GSSetColourRenderingIntent, GSSetFlatnessTolerance, GSSetLineCap, GSSetLineDashPattern, GSSetLineJoin, GSSetLineWidth, GSSetMiterLimit, GSSetParameters)
  )
import Data.PDF.PDFObject (PDFObject (PDFDictionary))
import Data.PDF.PDFWork (PDFWork)
import Data.PDF.WorkData (WorkData (wNameTranslations, wAdditionalGStates, wPDF))
import PDF.Object.Object.FromPDFObject (fromPDFObject)
import Data.PDF.PDFPartition (PDFPartition (ppObjectsWithStream, ppObjectsWithoutStream))
import PDF.Object.Object.Properties (hasKey)
import Data.PDF.Program (Program, extractObjects)
import Data.PDF.Resource (Resource (ResExtGState), resName, toNameBase)
import Data.Sequence (Seq (Empty, (:<|)), spanl)
import Data.Sequence qualified as SQ

{-|
Test if a command can be factorized into an ExtGState resource.

A factorizable command is one that sets a graphics state parameter that can be
stored in an ExtGState dictionary. These include:

* Line width (w operator)
* Line cap style (J operator)
* Line join style (j operator)
* Miter limit (M operator)
* Line dash pattern (d operator)
* Color rendering intent (ri operator)
* Flatness tolerance (i operator)

Font selection remains a Tf command: ExtGState Font entries require a font
object reference, whereas Tf uses a name in the local resource dictionary. Other
commands are not factorizable and must be applied directly.
-}
isFactorizable :: Command -> Bool
isFactorizable (Command operator (GFXNumber _value :<| Empty)) =
  operator `elem` [GSSetLineWidth, GSSetLineCap, GSSetLineJoin,
                  GSSetMiterLimit, GSSetFlatnessTolerance]
isFactorizable (Command GSSetColourRenderingIntent (GFXName _intent :<| Empty)) = True
isFactorizable (Command GSSetLineDashPattern
    (GFXArray values :<| GFXNumber _phase :<| Empty)) = all isNumber values
 where
  isNumber GFXNumber{} = True
  isNumber _other = False
isFactorizable _other = False

-- | Uncompressed serialized cost, including the resource dictionary wrapper.
-- Compression and indirect-object layout are deliberately not predicted here.
gStateCost :: WorkData -> Program -> Int
gStateCost work program = programCost program + resourceCopies work * resourceCost
 where
  resourceCost
    | Map.null (wAdditionalGStates work) = 0
    | otherwise = BS.length $ fromPDFObject $ PDFDictionary $
        Map.singleton "ExtGState" (PDFDictionary (wAdditionalGStates work))

-- | The writer currently copies generated states into every resource scope.
-- Count that overhead conservatively; standalone programs use one scope.
resourceCopies :: WorkData -> Int
resourceCopies work = max 1 $ length $ filter (hasKey "Resources") $
  toList (ppObjectsWithStream (wPDF work))
    ++ toList (ppObjectsWithoutStream (wPDF work))

programCost :: Program -> Int
programCost = BS.length . separateGfx . extractObjects

-- | Plan resource allocation without mutating the document. Identical runs are
-- considered together so repeated settings can amortize the dictionary cost.
-- Each accepted rewrite must reduce the complete serialized cost.
planGState :: WorkData -> Program -> (Program, WorkData)
planGState initial program = foldl' choose (program, initial) dictionaries
 where
  runs :: Program -> [Program]
  runs Empty = []
  runs commands@(command :<| rest)
    | isFactorizable command
    = let
        run :: Program
        remaining :: Program
        (run, remaining) = spanl isFactorizable commands
      in
        run : runs remaining

    | otherwise
    = runs rest

  dictionaries :: [(PDFObject, [Int])]
  dictionaries = Map.toList $ Map.fromListWith (++)
    [ (PDFDictionary (mkExtGState run), [programCost run])
    | run <- runs program
    ]

  choose :: (Program, WorkData) -> (PDFObject, [Int]) -> (Program, WorkData)
  choose (current, work) (dictionary, runCosts) =
    let
      existing :: [ByteString]
      existing = [ existingName
                 | (existingName, value) <- Map.toList (wAdditionalGStates work)
                 , value == dictionary
                 ]

      resource :: Resource
      resource = case existing of
        existingName : _ -> ResExtGState existingName
        [] -> toNameBase (ResExtGState "") (Map.size (wNameTranslations work))

      name :: ByteString
      name = resName resource

      candidateWork :: WorkData
      candidateWork = work
        { wAdditionalGStates = Map.insert name
                                          dictionary
                                          (wAdditionalGStates work)
        , wNameTranslations = Map.insert resource
                                         resource
                                         (wNameTranslations work)
        }

      replacement :: Program
      replacement = SQ.singleton (mkCommand GSSetParameters [GFXName name])

      rewrite :: Program -> Program
      rewrite Empty = Empty
      rewrite commands@(command :<| rest)
        | isFactorizable command
        =let
            run :: Program
            remaining :: Program
            (run, remaining) = spanl isFactorizable commands

            rewritten :: Program
            rewritten =
              if PDFDictionary (mkExtGState run) == dictionary
                  && programCost replacement < programCost run
                then replacement
                else run
          in
            rewritten <> rewrite remaining

        | otherwise
        = SQ.singleton command <> rewrite rest

      candidate :: Program
      candidate = rewrite current

      -- Cheap upper bound avoids rescanning the stream for every unique, short
      -- run. Full serialization still decides borderline candidates.
      savings :: Int
      savings = sum [ max 0 (cost - programCost replacement)
                    | cost <- runCosts
                    ]

      entryCost :: Int
      entryCost =
        if null existing
          then
            BS.length (fromPDFObject (PDFDictionary (Map.singleton name
                                                                   dictionary
                                                    )
                                     )
                      ) - 4
          else
            0
    in
      if savings > resourceCopies work * entryCost
         && gStateCost candidateWork candidate < gStateCost work current
        then (candidate, candidateWork)
        else (current, work)

-- | Commit only profitable resource replacements, preserving all barriers.
optimizeGState :: Logging m => Program -> PDFWork m Program
optimizeGState program = do
  work <- get
  let
    optimized :: Program
    updated :: WorkData
    (optimized, updated) = planGState work program

  put updated
  return optimized
