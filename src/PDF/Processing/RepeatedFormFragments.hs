{-|
Share repeated, bounded painting fragments using ordinary Form XObjects.

This document pass deliberately accepts a small, auditable subset: leaf q/Q
blocks on pages, containing filled paths and opaque device-colour images.
Leading matrices stay at the call site. Text, strokes, internal matrices,
clipping, patterns, nested forms and transparency are not extracted. Tagged PDFs
and pages with transparency state or marked content are left alone.

A form inherits the caller's CTM and clipping; its explicit device colours and
local resources prevent accidental dependence on a parent's resource scope. The
original q/Q wrappers remain, and paths must be empty at both boundaries. See
ISO 32000-1, 8.10.2 (form execution) and 11.6 (transparency).
-}
module PDF.Processing.RepeatedFormFragments (repeatedFormFragments) where

import Control.Monad (foldM, guard, unless, when)
import Control.Monad.State (get, gets, put)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Either (fromRight)
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Kind (Type)
import Data.List (foldl', sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust)
import Data.PDF.Command (Command (Command), mkCommand)
import Data.PDF.GFXObject
  ( GFXObject (GFXName, GFXNumber)
  , GSOperator (GSBeginInlineImage, GSBeginMarkedContentSequence, GSBeginMarkedContentSequencePL, GSBeginText, GSCloseFillStrokeEOR, GSCloseFillStrokeNZWR, GSCloseStrokePath, GSCloseSubpath, GSCubicBezierCurve, GSCubicBezierCurve1To, GSCubicBezierCurve2To, GSEndMarkedContentSequence, GSEndPath, GSEndText, GSFillPathEOR, GSFillPathNZWR, GSFillStrokePathEOR, GSFillStrokePathNZWR, GSLineTo, GSMarkedContentPoint, GSMarkedContentPointPL, GSMoveTo, GSPaintXObject, GSRectangle, GSRestoreGS, GSSaveGS, GSSetCTM, GSSetNonStrokeCMYKColorspace, GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetParameters, GSSetTextRenderingMode, GSStrokePath, GSUnknown)
  , separateGfx
  )
import Data.PDF.OptimizationType (OptimizationType (GfxOptimization))
import Data.PDF.PDFObject
  ( PDFObject (PDFArray, PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNumber, PDFReference, PDFTrailer, PDFXRefStream)
  , mkPDFArray
  )
import Data.PDF.PDFPartition
  (PDFPartition (ppObjectsWithStream, ppObjectsWithoutStream, ppTrailers))
import Data.PDF.PDFWork
  (PDFWork, getReference, lastObjectNumber, putObject, sayComparisonP, tryP)
import Data.PDF.Program (Program, extractObjects, parseProgram)
import Data.PDF.Resource (Resource (ResXObject))
import Data.PDF.Settings (OptimizeGFX (OptimizeGFX), Settings (sOptimizeGFX))
import Data.PDF.WorkData (WorkData (wPDF, wSettings))
import Data.Sequence (Seq)
import Data.Sequence qualified as SQ
import Data.Set (Set)
import Data.Set qualified as Set

import PDF.Graphics.Interpreter.RenameResources (renameResources)
import PDF.Graphics.Parser.Stream (gfxParse)
import PDF.Object.Container (getFilters)
import PDF.Object.Object.FromPDFObject (fromPDFObject)
import PDF.Object.Object.Properties (getValueForKey)
import PDF.Object.State (getStream, setStream, setValue)
import PDF.Processing.Filter (filterOptimize)
import PDF.Processing.Unfilter (unfilter)

import Util.Dictionary (Dictionary)
import Data.Map.Strict (Map)

-- | A repeated fragment found inside a page's graphics program.
--
-- Positions refer to the body inside q/Q, excluding leading cm operators.
type Occurrence :: Type
data Occurrence = Occurrence
  { occurrencePage      :: !Int
  , occurrenceStart     :: !Int
  , occurrenceLength    :: !Int
  , occurrenceBody      :: !ByteString
  , occurrenceResources :: !(Dictionary PDFObject)
  , occurrenceBounds    :: ![Double]
  }

-- | The page data needed while analyzing and rewriting a page.
type Page :: Type
data Page = Page
  { pageObject    :: !PDFObject
  , pageStream    :: !PDFObject
  , pageDecoded   :: !PDFObject
  , pageProgram   :: !Program
  , pageResources :: !(Dictionary PDFObject)
  , pageXObjects  :: !(Dictionary PDFObject)
  }

-- | Serialize a parsed graphics program back to PDF graphics commands.
serialize :: Program -> ByteString
serialize = separateGfx . extractObjects

-- | Resolve dictionary wrappers while preserving references stored in values.
--
-- The visited set prevents malformed cyclic reference graphs from recursing
-- forever. Resource values remain references, so identity and sharing are
-- preserved.
dictionary :: PDFObject -> PDFWork IO (Maybe (Dictionary PDFObject))
dictionary = go Set.empty
 where
  -- Follow indirect references until a dictionary or a cycle is reached.
  go :: Set Int -> PDFObject -> PDFWork IO (Maybe (Dictionary PDFObject))
  go seen (PDFReference n v)
    | Set.member n seen
    = pure Nothing

    | otherwise
    = getReference (PDFReference n v) >>= go (Set.insert n seen)

  go seen (PDFIndirectObject _ _ value) = go seen value

  go _ (PDFDictionary value) = pure (Just value)

  go _ _ = pure Nothing

-- | Find a page's inherited resources, following its parent chain safely.
inheritedResources :: PDFObject -> PDFWork IO (Maybe (Dictionary PDFObject))
inheritedResources = go Set.empty
 where
  -- Search the current object and then its parents for a Resources entry.
  go :: Set Int -> PDFObject -> PDFWork IO (Maybe (Dictionary PDFObject))
  go seen object = case getValueForKey "Resources" object of
    Just value -> dictionary value

    Nothing -> case getValueForKey "Parent" object of
      Just (PDFReference n v) | Set.notMember n seen ->
        getReference (PDFReference n v) >>= go (Set.insert n seen)

      Nothing -> pure (Just Map.empty)

      _       -> pure Nothing

-- | Collect every indirect object reference contained in a PDF object.
references :: PDFObject -> [Int]
references (PDFReference n _) =
  [n]

references (PDFIndirectObject _ _ value) =
  references value

references (PDFDictionary values) =
  concatMap references values

references (PDFArray values) =
  concatMap references values

references (PDFIndirectObjectWithStream _ _ values _) =
  concatMap references values

references (PDFTrailer value) =
  references value

references (PDFXRefStream _ _ values _) =
  concatMap references values

references _ =
  []

-- | Whether an operator makes a graphics block unsafe to extract.
--
-- These operators can change the interpretation of extracted content even
-- outside the block, or indicate syntax whose effects we cannot prove.
unsafeContext :: Command -> Bool
unsafeContext (Command operator _) = case operator of
  GSSetParameters                -> True
  GSBeginMarkedContentSequence   -> True
  GSBeginMarkedContentSequencePL -> True
  GSEndMarkedContentSequence     -> True
  GSMarkedContentPoint           -> True
  GSMarkedContentPointPL         -> True
  GSSetTextRenderingMode         -> True
  GSBeginInlineImage             -> True
  GSUnknown _                    -> True
  _                              -> False

-- | Whether an operator starts or extends a current path.
pathStart :: GSOperator -> Bool
pathStart operator = operator `elem`
  [ GSMoveTo
  , GSLineTo
  , GSCubicBezierCurve
  , GSCubicBezierCurve1To
  , GSCubicBezierCurve2To
  , GSCloseSubpath
  , GSRectangle
  ]

-- | Whether an operator consumes or abandons the current path.
pathEnd :: GSOperator -> Bool
pathEnd operator = operator `elem`
  [ GSStrokePath
  , GSCloseStrokePath
  , GSFillPathNZWR
  , GSFillPathEOR
  , GSFillStrokePathNZWR
  , GSFillStrokePathEOR
  , GSCloseFillStrokeNZWR
  , GSCloseFillStrokeEOR
  , GSEndPath
  ]

-- | Find leaf q/Q blocks that contain neither text nor an open path.
--
-- Balancing is validated while the current path is tracked independently of
-- q/Q: paths are not part of the saved graphics state. Only leaf blocks are
-- candidates.
blocks :: Program -> Maybe [(Int, Int)]
blocks program = go 0 [] False False [] (toList program)
 where
  -- Walk the program while recording save/restore nesting and syntax state.
  go
    :: Int
    -> [(Int, Bool)]
    -> Bool
    -> Bool
    -> [(Int, Int)]
    -> [Command]
    -> Maybe [(Int, Int)]
  go _ [] False False found [] = Just (reverse found)

  go _ _ _ _ _ [] = Nothing

  go index stack path text found (Command operator parameters : rest)
    -- A block is eligible only when it starts outside text and a path.
    | operator == GSSaveGS && SQ.null parameters
    = go (index + 1)
         ((index, not path && not text) : invalidateParent stack)
         path
         text
         found
         rest

    -- A restore closes the innermost block; unbalanced restores invalidate the
    -- program.
    | operator == GSRestoreGS && SQ.null parameters
    = case stack of
        (start, eligible) : parents ->
          let
            found' = if eligible && not path && not text
                      then (start + 1, index - start - 1) : found
                      else found
          in
            go (index + 1) parents path text found' rest

        [] -> Nothing

    -- Text may not be nested, because the accepted subset excludes text.
    | operator == GSBeginText
    = if text
        then Nothing
        else continue path True

    -- End of text block.
    | operator == GSEndText
    = if text
        then continue path False
        else Nothing

    -- Path state persists across q/Q, so it must be carried separately.
    | otherwise
    = continue (pathStart operator || (not (pathEnd operator) && path)) text
   where
    continue :: Bool -> Bool -> Maybe [(Int, Int)]
    continue path' text' = go (index + 1) stack path' text' found rest

  -- Mark the enclosing block unsafe when a nested block is encountered.
  invalidateParent :: [(Int, Bool)] -> [(Int, Bool)]
  invalidateParent ((n, _) : rest) = (n, False) : rest
  invalidateParent []              = []

-- | Read a page when it has the structure required by the optimization.
readPage :: IntMap Int -> PDFObject -> PDFWork IO (Maybe Page)
readPage counts page
  -- Skip non-page objects.
  | getValueForKey "Type" page /= Just (PDFName "Page")
  = pure Nothing

  -- Skip pages with certain entries that indicate complex structures.
  | any (\entry -> isJust (getValueForKey entry page))
        ["Group", "StructParents", "PresSteps"]
  = pure Nothing

  -- Process pages with a single content stream.
  | otherwise
  = case getValueForKey "Contents" page of
      -- Handle the case where the page has a single content stream.
      Just ref@(PDFReference n _) | IM.lookup n counts == Just 1 -> do
        stream <- getReference ref

        case stream of
          -- Only process indirect objects with streams that do not have certain
          -- dictionary entries.
          PDFIndirectObjectWithStream _ _ dict _ | all (`Map.notMember` dict) ["Subtype", "Type", "StructParents", "F"] -> do
            decoded <- unfilter stream
            filters <- getFilters decoded
            bytes <- getStream decoded
            resources <- inheritedResources page

            case (null filters, gfxParse bytes, resources) of
              -- Only consider streams that have no filters, have been
              -- successfully parsed, and have inherited resources.
              (True, Right objects, Just res) -> do
                xobjects <- maybe (pure (Just Map.empty))
                                  dictionary
                                  (Map.lookup "XObject" res)

                colors <- maybe (pure (Just Map.empty))
                                dictionary
                                (Map.lookup "ColorSpace" res)

                -- Parse the decoded stream once; all later checks operate on
                -- the command sequence rather than on raw bytes.
                let program = parseProgram objects

                pure $ do
                  xs <- xobjects
                  cs <- colors

                  guard (all (`Map.notMember` cs)
                        ["DefaultGray", "DefaultRGB", "DefaultCMYK"])

                  guard (  SQ.length program <= 100000
                        && not (any unsafeContext program)
                        )

                  _ <- blocks program

                  pure (Page page stream decoded program res xs)
              _ -> pure Nothing
          _ -> pure Nothing
      _ -> pure Nothing

-- | Check that an XObject is an opaque image with a device colour space.
--
-- Form XObjects (including recursive forms) and images with masks or other
-- potentially visible side effects are explicitly excluded.
opaqueImage :: PDFObject -> PDFWork IO Bool
opaqueImage reference@PDFReference{} = do
  object <- getReference reference

  pure $ case object of
    -- Only consider indirect objects with streams that are images and have a
    -- device colour space.
    PDFIndirectObjectWithStream _ _ dict _ ->
      Map.lookup "Subtype" dict == Just (PDFName "Image")
      && Map.lookup "ColorSpace" dict `elem`
         map (Just . PDFName) ["DeviceGray", "DeviceRGB", "DeviceCMYK"]
      && all (`Map.notMember` dict)
             [ "Mask"
             , "SMask"
             , "SMaskInData"
             , "ImageMask"
             , "Alternates"
             , "OPI"
             , "OC"
             , "StructParent"
             ]
  
    -- Any other indirect object with a stream is not considered an opaque
    -- image.
    _ -> False

opaqueImage _ = pure False

-- | Compute a conservative bounding box for a supported fragment.
--
-- Bounds include all path control points (a conservative Bezier hull). There
-- are no strokes, so line widths/joins cannot enlarge these bounds.
bounds :: Program -> Maybe [Double]
bounds program = do
  -- Fold the fragment while enforcing the supported drawing grammar.
  (points, path, _, painted) <- foldM step
                                      ([], False, False, False)
                                      (toList program)

  guard (not path && painted && not (null points))

  let
    xs :: [Double]
    xs = map fst points

    ys :: [Double]
    ys = map snd points

  pure [minimum xs - 1, minimum ys - 1, maximum xs + 1, maximum ys + 1]
 where
  -- Extract finite, reasonably sized numeric operands.
  numbers :: Seq GFXObject -> Maybe [Double]
  numbers args = traverse
    ( \case
        GFXNumber n | not (isNaN n || isInfinite n) && abs n < 1e12 ->
          Just n

        _ ->
          Nothing
    )
    (toList args)

  -- Pair successive coordinates, as required by curve operands.
  pairs :: [Double] -> [(Double, Double)]
  pairs (x : y : rest) = (x, y) : pairs rest
  pairs _              = []

  -- Consume one command and update path, colour, and painting state.
  step
    :: ([(Double, Double)], Bool, Bool, Bool)
    -> Command
    -> Maybe ([(Double, Double)], Bool, Bool, Bool)
  step (points, path, color, painted) (Command operator args) = do
    ns <- if operator == GSPaintXObject
            then Just []
            else numbers args

    case (operator, ns) of
      (GSSetNonStrokeGrayColorspace, [_]) ->
        pure (points, path, True, painted)

      (GSSetNonStrokeRGBColorspace, [_, _, _]) ->
        pure (points, path, True, painted)

      (GSSetNonStrokeCMYKColorspace, [_, _, _, _]) ->
        pure (points, path, True, painted)

      (GSRectangle, [x, y, w, h]) ->
        pure ((x, y) : (x + w, y + h) : points, True, color, painted)

      (GSMoveTo, [x, y]) ->
        pure ((x,y):points, True, color, painted)

      (GSLineTo, [x, y]) | path ->
        pure ((x,y):points, True, color, painted)

      (GSCubicBezierCurve, [_, _, _, _, _, _]) | path ->
        pure (pairs ns ++ points, True, color, painted)

      (GSCubicBezierCurve1To, [_, _, _, _]) | path ->
        pure (pairs ns ++ points, True, color, painted)

      (GSCubicBezierCurve2To, [_, _, _, _]) | path ->
        pure (pairs ns ++ points, True, color, painted)

      (GSCloseSubpath, []) | path ->
        pure (points, path, color, painted)

      (GSFillPathNZWR, []) | path && color ->
        pure (points, False, color, True)

      (GSFillPathEOR, []) | path && color ->
        pure (points, False, color, True)

      (GSEndPath, []) ->
        pure (points, False, color, painted)

      (GSPaintXObject, []) | not path -> case toList args of
        -- Images contribute a minimal non-empty box because their matrix is
        -- deliberately left at the call site.
        [GFXName _] -> pure ((0,0):(1,1):points, False, color, True)
        _           -> Nothing

      _ -> Nothing

-- | Test whether a command is a finite six-number transformation matrix.
isMatrix :: Command -> Bool
isMatrix (Command GSSetCTM args) =
  length args == 6 && all finiteNumber args
 where
  -- Reject non-numeric, NaN, and infinite matrix operands.
  finiteNumber :: GFXObject -> Bool
  finiteNumber (GFXNumber n) = not (isNaN n || isInfinite n)
  finiteNumber _             = False

isMatrix _ = False

-- | Extract all eligible repeated fragments from one page.
occurrences :: Int -> Page -> PDFWork IO [Occurrence]
occurrences pageNumber page =
  catMaybes
    <$> mapM candidate (maybe [] (take 2048) (blocks (pageProgram page)))
 where
  -- Analyze one candidate q/Q block and normalize its resources.
  candidate :: (Int, Int) -> PDFWork IO (Maybe Occurrence)
  candidate (start, count) = do
    let
      original :: Seq Command
      original = SQ.take count (SQ.drop start (pageProgram page))

      matrices :: Seq Command
      body :: Seq Command
      (matrices, body) = SQ.spanl isMatrix original

      names :: [ByteString]
      names = [ name
              | Command GSPaintXObject args <- toList body
              , GFXName name <- toList args
              ]

      uniqueNames :: [ByteString]
      uniqueNames = Set.toAscList (Set.fromList names)

    case bounds body of
      Nothing -> pure Nothing

      Just box | BS.length (serialize body) >= 64 -> do
        -- Resolve every referenced image and reject the candidate if any image
        -- is missing or has a non-local visual effect.
        let values = traverse (`Map.lookup` pageXObjects page) uniqueNames

        valid <- maybe (pure False) (fmap and . mapM opaqueImage) values

        pure $ do
          resources <- values
          guard valid

          -- Assign names by referenced object identity, not source spelling.
          let
            -- Assign stable local names from object identity, so two source
            -- names referring to the same image cannot produce conflicting
            -- resource entries.
            ordered :: [ByteString]
            ordered = Set.toAscList (Set.fromList (map fromPDFObject resources))

            -- Map original resource names to stable local names based on object
            -- identity.
            localName :: PDFObject -> ByteString
            localName value = "I"
                           <> BC.pack (show (length
                                (takeWhile (< fromPDFObject value) ordered)
                              ))

            -- Create a mapping from original resource names to stable local
            -- names.
            translations :: Map Resource Resource
            translations = Map.fromList
              [ (ResXObject old
              , ResXObject (localName value))
              | (old, value) <- zip uniqueNames resources
              ]

            -- Create a local mapping from stable local names to their
            -- corresponding PDF objects.
            local :: Map ByteString PDFObject
            local = Map.fromList [ (localName value, value)
                                 | value <- resources
                                 ]

          pure (Occurrence pageNumber
                           (start + SQ.length matrices)
                           (SQ.length body)
                           (serialize (renameResources translations body))
                           local
                           box
               )

      _ -> pure Nothing

-- | Grouping key for fragments with identical normalized content and resources.
key :: Occurrence -> (ByteString, ByteString)
key occurrence =
  ( occurrenceBody occurrence
  , fromPDFObject (PDFDictionary (occurrenceResources occurrence))
  )

-- | Return the serialized size of a PDF object.
objectSize :: PDFObject -> Int
objectSize = BS.length . fromPDFObject

-- | Apply one group of occurrences if the fully encoded result is smaller.
--
-- Each proposal is fully compressed before committing. Its estimate includes
-- the form dictionary, local resources, changed page dictionaries, call
-- streams, and a reserve for xref, object-stream, and trailer growth.
applyGroup :: IntMap Page -> [Occurrence] -> PDFWork IO ()
applyGroup pages group@(first : _)
  | length group >= 2 = do
      -- Keep a rollback point because encoding or writing any part of the
      -- proposal may fail.
      snapshot <- get
      result <- tryP $ do
        number <- (+ 1) <$> lastObjectNumber

        let
          -- Build the dictionary for the shared form XObject.
          dict :: Map ByteString PDFObject
          dict = Map.fromList
            [ ("Type", PDFName "XObject"), ("Subtype", PDFName "Form")
            , ("BBox", mkPDFArray (map PDFNumber (occurrenceBounds first)))
            , ( "Resources"
              , PDFDictionary (if Map.null (occurrenceResources first)
                                then
                                  Map.empty
                                else
                                  Map.singleton "XObject"
                                    (PDFDictionary (occurrenceResources first)))
              )
            ]

        -- Build and compress the shared form before measuring the proposal.
        form <- setStream (occurrenceBody first)
                          (PDFIndirectObjectWithStream number 0 dict "")
                >>= filterOptimize GfxOptimization

        -- Replace occurrences page by page and collect both old and new data
        -- for the size comparison.
        changes <- mapM (changePage number)
                        (Map.toList
                          (Map.fromListWith
                            (++)
                            [(occurrencePage o, [o]) | o <- group]
                          )
                        )

        let
          -- Calculate the total size of the old page objects and call streams.
          before :: Int
          before = sum [ objectSize oldPage + objectSize oldStream
                       | (oldPage, oldStream, _, _) <- changes
                       ]

          -- Calculate the total size of the new page objects and call streams.
          after :: Int
          after = objectSize form + 64
                + sum [ objectSize newPage + objectSize newStream
                      | (_, _, newPage, newStream) <- changes
                      ]

        -- Commit only strict size improvements; equal-sized rewrites add no
        -- value and can still alter object layout.
        if after < before then do
          putObject form
          mapM_ (\(_, _, p, s) -> putObject p >> putObject s) changes

          sayComparisonP "Repeated form fragments" before after

          pure True
        else
          pure False

      case result of
        Right True -> pure ()
        _          -> put snapshot
 where
  -- Build the updated page objects and call stream for one page.
  changePage
    :: Int
    -> (Int, [Occurrence])
    -> PDFWork IO (PDFObject, PDFObject, PDFObject, PDFObject)
  changePage number (pageNumber, uses) = do
    let
      -- Extract the current page from the page map.
      page :: Page
      page = pages IM.! pageNumber

      -- Generate a fresh resource name for the XObject.
      name :: ByteString
      name = freshName (pageXObjects page) 0

      -- Build the call program for the XObject.
      call :: Program
      call = SQ.singleton (mkCommand GSPaintXObject [GFXName name])

      -- Replace occurrences of the original content with the XObject call.
      replace :: Program -> Occurrence -> Program
      replace current occurrence =
        -- Process occurrences from right to left so earlier offsets remain
        -- valid while the program is being rebuilt.
        SQ.take (occurrenceStart occurrence) current
        <> call
        <> SQ.drop (occurrenceStart occurrence + occurrenceLength occurrence)
                   current
      -- Build the final program by applying all replacements.
      program :: Program
      program = foldl' replace
                       (pageProgram page)
                       (sortOn (negate . occurrenceStart) uses)

      -- Build the updated resources dictionary for the page.
      resources :: Map ByteString PDFObject
      resources = Map.insert "XObject"
                             (PDFDictionary
                              (Map.insert name
                                          (PDFReference number 0)
                                          (pageXObjects page)
                              )
                             )
                             (pageResources page)

    -- Serialize the updated program and create a new stream for the page.
    stream <- setStream (serialize program) (pageDecoded page)
              >>= filterOptimize GfxOptimization

    updated <- setValue "Resources" (PDFDictionary resources) (pageObject page)

    pure (pageObject page, pageStream page, updated, stream)

applyGroup _ _ = pure ()

-- | Choose the first unused page-local resource name with the RF prefix.
freshName :: Dictionary PDFObject -> Int -> ByteString
freshName resources index =
  let
    name :: ByteString
    name = "RF" <> BC.pack (show index)
  in
    if Map.member name resources
      then freshName resources (index + 1)
      else name

-- | Read a page without allowing malformed objects to abort the optimization.
safeReadPage :: IntMap Int -> PDFObject -> PDFWork IO (Maybe Page)
safeReadPage counts object = do
  result <- tryP (readPage counts object)
  pure (fromRight Nothing result)

-- | Replace repeated eligible fragments throughout the current PDF.
repeatedFormFragments :: PDFWork IO ()
repeatedFormFragments = do
  enabled <- gets ((== OptimizeGFX) . sOptimizeGFX . wSettings)
  when enabled $ do
    -- Gather all objects once to detect shared references and tagged PDFs.
    pdf <- gets wPDF
    let
      -- Collect all objects from the PDF for analysis.
      objects :: [PDFObject]
      objects = IM.elems (ppObjectsWithoutStream pdf)
             ++ IM.elems (ppObjectsWithStream pdf)
             ++ toList (ppTrailers pdf)

      -- Determine if the PDF is tagged by checking for a StructTreeRoot entry.
      tagged :: Bool
      tagged = any (isJust . getValueForKey "StructTreeRoot") objects

      -- Count the number of references to each object in the PDF.
      counts :: IntMap Int
      counts = IM.fromListWith (+) [ (n, 1)
                                   | object <- objects
                                   , n <- references object
                                   ]

    unless tagged $ do
      -- A candidate page must have a uniquely referenced contents stream.
      candidates <- mapM
        (\(n, object) -> fmap ((n,) <$>) (safeReadPage counts object))
        (IM.toList (ppObjectsWithoutStream pdf))

      let
        -- Build a map from object numbers to their corresponding Page objects.
        pages :: IntMap Page
        pages = IM.fromList (catMaybes candidates)

      found <- concat <$> mapM (uncurry occurrences) (IM.toList pages)

      let
        -- Group identical normalized fragments; groups with one occurrence do
        -- not justify creating a form.
        groups :: [[Occurrence]]
        groups =
          filter ((>= 2) . length)
                 (Map.elems (Map.fromListWith (++) [(key o, [o]) | o <- found]))

      -- Re-read pages after each accepted group, so offsets and resource names
      -- never come from a stale plan. New forms are not themselves candidates.
      mapM_ (applyFresh counts) groups
 where
  applyFresh :: IntMap Int -> [Occurrence] -> PDFWork IO ()
  applyFresh counts group = do
    -- Only pages touched by this group need to be parsed again.
    let involved = Set.toList (Set.fromList (map occurrencePage group))

    -- Re-read the involved pages to ensure we have the latest versions.
    fresh <- mapM (\n -> do
      object <- getReference (PDFReference n 0)
      page <- safeReadPage counts object
      pure ((n,) <$> page)) involved

    let pages = IM.fromList (catMaybes fresh)

    -- Recompute offsets and resources after previous accepted rewrites. This
    -- ensures that any changes made by earlier groups are reflected in the
    -- current analysis.
    found <- concat <$> mapM (uncurry occurrences) (IM.toList pages)

    case group of
      first : _ -> applyGroup pages (filter ((== key first) . key) found)
      []        -> pure ()
