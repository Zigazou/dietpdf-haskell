-- | Resolve document resources once before optimizing individual streams.
module PDF.Document.ResourceContext
  ( StreamResources
  , buildStreamResources
  , inherited
  , resolve
  , value
  ) where

import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.PDF.PDFObject
  ( PDFObject (PDFArray, PDFDictionary, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFReference)
  )

import PDF.Object.Object.Properties (getValueForKey)

import Util.Dictionary (Dictionary)
import Data.IntSet (IntSet)

-- | Nothing means unresolved or conflicting owners. Absence means no known owner.
type StreamResources :: Type
type StreamResources = IntMap (Maybe (Dictionary PDFObject))

{- | Follow references without expanding dictionaries or changing stream
objects. Both object number and generation must match; cycles are rejected.
-}
resolve :: IntMap PDFObject -> PDFObject -> Maybe PDFObject
resolve objects = go IS.empty
 where
  go seen (PDFReference number generation)
    | IS.member number seen = Nothing
    | otherwise = do
        object <- IM.lookup number objects
        case object of
          PDFIndirectObject n g inner
            | n == number && g == generation -> go (IS.insert number seen) inner
          PDFIndirectObjectWithStream n g _ _
            | n == number && g == generation -> Just object
          _ -> Nothing
  go seen (PDFIndirectObject _ _ inner) = go seen inner
  go _ object = Just object

{- | Resolve a dictionary value for the given key in a PDF object graph.
-}
value :: IntMap PDFObject -> ByteString -> PDFObject -> Maybe PDFObject
value objects key object = getValueForKey key object >>= resolve objects

{- | Look up an inherited dictionary key, following the parent chain when
needed.
-}
inherited :: IntMap PDFObject -> ByteString -> PDFObject -> Maybe PDFObject
inherited objects key = go IS.empty
 where
  go seen object = case getValueForKey key object of
    Just entry -> resolve objects entry
    Nothing -> case getValueForKey "Parent" object of
      Just reference@(PDFReference number _)
        | not (IS.member number seen) ->
            resolve objects reference >>= go (IS.insert number seen)
      _ -> Nothing

-- | Index page contents and recursively reachable Form XObjects. Page resources
-- follow the parent chain; forms use their own resources when present and the
-- caller's otherwise. All listed forms are visited conservatively, even if no
-- Do operator invokes them. Resource entries retain their indirect references;
-- only the outer Resources dictionary is resolved here.
--
-- Scanning Page objects also covers disconnected pages without requiring a
-- well-formed catalog. Conflicts are sticky, and revisiting a form with the same
-- effective context is skipped, bounding recursive resource graphs.
buildStreamResources :: IntMap PDFObject -> StreamResources
buildStreamResources objects = fst $ foldl' visitPage (IM.empty, IM.empty) objects
 where
  dictionary entry = case entry of
    Just (PDFDictionary entries) -> Just entries
    _                            -> Nothing

  visitPage state page
    | getValueForKey "Type" page == Just (PDFName "Page") =
        let resources = dictionary (inherited objects "Resources" page)
            withContents = maybe state (contents IS.empty resources state)
                                     (getValueForKey "Contents" page)
        in forms resources withContents
    | otherwise = state

  record
    :: Int
    -> Maybe (Dictionary PDFObject)
    -> StreamResources
    -> StreamResources
  record = IM.insertWith agree

  agree
    :: Maybe (Dictionary PDFObject)
    -> Maybe (Dictionary PDFObject)
    -> Maybe (Dictionary PDFObject)
  agree a b | a == b = a
            | otherwise = Nothing

  contents
    :: IntSet
    -> Maybe (Dictionary PDFObject)
    -> (StreamResources, IntMap [Maybe (Dictionary PDFObject)])
    -> PDFObject
    -> (StreamResources, IntMap [Maybe (Dictionary PDFObject)])
  contents seen resources state@(result, visited) entry = case entry of
    PDFReference number _
      | IS.member number seen -> state
      | otherwise -> maybe state
                           (contents (IS.insert number seen) resources state)
                           (resolve objects entry)

    PDFArray entries -> foldl' (contents seen resources) state entries

    PDFIndirectObjectWithStream number _ _ _ ->
      (record number resources result, visited)

    _ -> state

  forms
    :: Maybe (Dictionary PDFObject)
    -> (StreamResources, IntMap [Maybe (Dictionary PDFObject)])
    -> (StreamResources, IntMap [Maybe (Dictionary PDFObject)])
  forms Nothing state = state
  forms (Just resources) state =
    case Map.lookup "XObject" resources >>= resolve objects of
      Just (PDFDictionary entries) -> foldl' (form (Just resources))
                                             state
                                             entries

      _ -> state

  form
    :: Maybe (Dictionary PDFObject)
    -> (StreamResources, IntMap [Maybe (Dictionary PDFObject)])
    -> PDFObject
    -> (StreamResources, IntMap [Maybe (Dictionary PDFObject)])
  form caller state@(result, visited) entry = case resolve objects entry of
    Just object@(PDFIndirectObjectWithStream number _ _ _)
      | getValueForKey "Subtype" object == Just (PDFName "Form") ->
          let
            resources :: Maybe (Dictionary PDFObject)
            resources = case getValueForKey "Resources" object of
              Nothing  -> caller
              Just own -> dictionary (resolve objects own)

            previous :: [Maybe (Dictionary PDFObject)]
            previous = IM.findWithDefault [] number visited
          in
            if resources `elem` previous
              then state
              else forms resources
                    (record number resources result,
                      IM.insert number (resources : previous) visited)
    _ -> state
