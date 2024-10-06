module Pdf.Document.Encode
  ( -- * Encoding
    pdfEncode
    -- * XRef generation
  , calcOffsets
  , encodeObject
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExcept, runExceptT, throwE)

import Data.ByteString qualified as BS
import Data.Context (Context, Contextual (ctx))
import Data.Fallible (FallibleT, tryF)
import Data.Functor ((<&>))
import Data.IntMap qualified as IM
import Data.Logging (Logging, sayComparisonF, sayErrorF, sayF)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Sequence qualified as SQ
import Data.Text qualified as T
import Data.UnifiedError
    ( UnifiedError (EncodeEncrypted, EncodeNoIndirectObject, EncodeNoTrailer, EncodeNoVersion)
    )

import GHC.IO.Handle (BufferMode (LineBuffering))

import Pdf.Document.Document (PDFDocument, cFilter, fromList, singleton)
import Pdf.Document.EncodedObject (EncodedObject (EncodedObject), eoBinaryData)
import Pdf.Document.ObjectStream (explodeDocument, explodeList, insert)
import Pdf.Document.Partition
    ( PDFPartition (PDFPartition, ppHeads, ppObjectsWithStream, ppObjectsWithoutStream, ppTrailers)
    , lastTrailer
    , removeUnused
    )
import Pdf.Document.PDFObjects (findLast, fromPDFDocument)
import Pdf.Document.XRef (calcOffsets, xrefStreamTable)
import Pdf.Object.Object.FromPDFObject (fromPDFObject)
import Pdf.Object.Object.PDFObject
    ( PDFObject (PDFDictionary, PDFEndOfFile, PDFIndirectObject, PDFIndirectObjectWithStream, PDFName, PDFNull, PDFObjectStream, PDFReference, PDFStartXRef, PDFTrailer, PDFVersion, PDFXRefStream)
    )
import Pdf.Object.Object.Properties
    ( getObjectNumber
    , getValueForKey
    , hasKey
    , hasStream
    , isHeader
    , isIndirect
    , isTrailer
    )
import Pdf.Object.Optimize (optimize)
import Pdf.Object.State (getValue, setMaybe)

import System.IO (hSetBuffering, stderr)

import Util.ByteString (renameStrings)
import Util.Dictionary (mkDictionary)
import Util.Sequence (mapMaybe)
import Pdf.Object.Object.RenameResources (renameResources)

{- |
Encodes a PDF object and keeps track of its number and length.

Returns an `EncodedObject` which contains the object's number, the length of its
byte representation, the byte data, and any embedded objects.
-}
encodeObject :: Logging m => PDFObject -> FallibleT m EncodedObject
encodeObject object@(PDFIndirectObject number _ _) = return $
    EncodedObject number (BS.length bytes) bytes SQ.Empty
  where bytes = fromPDFObject object
encodeObject object@(PDFIndirectObjectWithStream number _ _ _) = return $
    EncodedObject number (BS.length bytes) bytes SQ.Empty
  where bytes = fromPDFObject object
encodeObject object@(PDFObjectStream number _ _ _) = do
  let bytes = fromPDFObject object
  embeddedObjects <- explodeList [object]
  return $ EncodedObject
            number
            (BS.length bytes)
            bytes
            (mapMaybe getObjectNumber (SQ.fromList embeddedObjects))

encodeObject object = return $ EncodedObject 0 (BS.length bytes) bytes SQ.Empty
  where bytes = fromPDFObject object


{- |
Updates an XRef stream object by copying certain fields ("Root", "Info", "ID")
from a given trailer object.

Returns the updated XRef stream object.
-}
updateXRefStm :: Logging m => PDFObject -> PDFObject -> FallibleT m PDFObject
updateXRefStm trailer xRefStm = do
  mRoot <- getValue "Root" trailer
  mInfo <- getValue "Info" trailer
  mID <- getValue "ID" trailer

  setMaybe "Root" mRoot xRefStm
    >>= setMaybe "Info" mInfo
    >>= setMaybe "ID" mID

{- |
Checks if the given PDF object is a Catalog object by verifying that its "Type"
key is set to "Catalog".

Returns `True` if it is a Catalog, `False` otherwise.
-}
isCatalog :: PDFObject -> Bool
isCatalog object@PDFIndirectObject{} =
  case runExcept (getValue "Type" object) of
    Left  _     -> False
    Right value -> value == Just (PDFName "Catalog")
isCatalog _anyOtherObject = False

{- |
Checks if the given PDF object contains document information (e.g., has an
"Author" key).

Returns `True` if the object contains document info, `False` otherwise.
-}
isInfo :: PDFObject -> Bool
isInfo object@PDFIndirectObject{} = hasKey "Author" object
isInfo _anyOtherObject            = False

{- |
Retrieves the trailer object from a `PDFPartition`. If a valid trailer object is
not present, it attempts to create one using the "Root" and "Info" references
from the partition.

Returns the final trailer object.
-}
getTrailer :: PDFPartition -> PDFObject
getTrailer partition = case lastTrailer partition of
  (PDFTrailer PDFNull) ->
    let
      catalog = findLast isCatalog (ppObjectsWithoutStream partition)
      info    = findLast isInfo (ppObjectsWithoutStream partition)
    in
      case (catalog, info) of
        ( Just (PDFIndirectObject cNumber cRevision _),
          Just (PDFIndirectObject iNumber iRevision _))
          -> PDFTrailer
            (PDFDictionary $ mkDictionary
              [ ("Root", PDFReference cNumber cRevision)
              , ("Info", PDFReference iNumber iRevision)
              ]
            )
        (Just (PDFIndirectObject cNumber cRevision _), Nothing) -> PDFTrailer
          ( PDFDictionary
          $ mkDictionary [("Root", PDFReference cNumber cRevision)]
          )
        _anyOtherCase -> PDFTrailer PDFNull
  (PDFXRefStream _ _ dict _) ->
    let catalog = Map.lookup "Root" dict
        info    = Map.lookup "Info" dict
    in  case (catalog, info) of
          (Just rCatalog, Just rInfo) ->
            PDFTrailer
              ( PDFDictionary
              $ mkDictionary [("Root", rCatalog), ("Info", rInfo)]
              )
          _anyOtherCase -> PDFTrailer PDFNull
  validTrailer -> validTrailer


{- |
Determines if a PDF object should be embedded in an object stream (ObjStm).

Returns `True` for indirect objects that do not have a stream.
-}
objectToEmbed :: PDFObject -> Bool
objectToEmbed object = isIndirect object && not (hasStream object)

{- |
Checks if a PDF object contains content (i.e., it has a stream but is not a
trailer).

Returns `True` if the object has a stream and is not a trailer.
-}
objectWithContent :: PDFObject -> Bool
objectWithContent object = hasStream object && not (isTrailer object)

{- |
Parallel map function that applies a given transformation to each element of a
traversable structure (e.g., list) using concurrency.

Returns the transformed elements wrapped in a `FallibleT`.
-}
pMapM
  :: (Logging IO, Traversable t)
  => (a -> FallibleT IO b)
  -> t a
  -> FallibleT IO (t b)
pMapM transform items =
  liftIO (mapConcurrently (runExceptT . transform) items)
    >>= either throwE return . sequence

{- |
Partitions a collection of PDF objects (`PDFDocument`) into a `PDFPartition`,
separating objects with streams, objects without streams, header objects, and
trailers.
-}
partitionDocument :: PDFDocument -> PDFPartition
partitionDocument objs =
  PDFPartition
    { ppObjectsWithStream    = fromPDFDocument $ cFilter objectWithContent objs
    , ppObjectsWithoutStream = fromPDFDocument $ cFilter objectToEmbed objs
    , ppHeads                = cFilter isHeader objs
    , ppTrailers             = cFilter isTrailer objs
    }


{- |
Cleans a `PDFPartition` by removing unused objects and logs the number of
removed objects.

Returns the cleaned partition. If an error occurs, logs the error and returns
the original partition.
-}
cleanPartition
  :: Logging m
  => Context
  -> PDFPartition
  -> FallibleT m PDFPartition
cleanPartition context partition =
  tryF (removeUnused partition) >>= \case
    Right unusedRemoved -> do
      let allCount = IM.size (ppObjectsWithStream partition)
                  + IM.size (ppObjectsWithoutStream partition)
          usedCount = IM.size (ppObjectsWithStream unusedRemoved)
                    + IM.size (ppObjectsWithoutStream unusedRemoved)
      sayComparisonF context "Unused objects removal" allCount usedCount
      return unusedRemoved
    Left theError -> do
      sayErrorF context "Unable to remove unused objects" theError
      return partition

{- |
Finds all resource names in a collection of PDF objects.

Resources are typically stored in a dictionary object with a "Resources" key.
-}
getAllResourceNames :: IM.IntMap PDFObject -> [BS.ByteString]
getAllResourceNames allObjects = concat (getAllResourceNames' allObjects)
 where
  getAllResourceNames' :: IM.IntMap PDFObject -> [[BS.ByteString]]
  getAllResourceNames' objects = do
    (_, object) <- IM.toList objects
    let resourceColorspaces = getResourceKeys "ColorSpace" object
        resourceFonts       = getResourceKeys "Font" object
        resourceXObjects    = getResourceKeys "XObject" object
        resourceExtGStates  = getResourceKeys "ExtGState" object
        resourceProperties  = getResourceKeys "Properties" object
        colorspaces         = getKeys "ColorSpace" object
        fonts               = getKeys "Font" object
        xObjects            = getKeys "XObject" object
        extGStates          = getKeys "ExtGState" object
        properties          = getKeys "Properties" object

    (return . concat . catMaybes)
      [ colorspaces
      , fonts
      , xObjects
      , extGStates
      , properties
      , resourceColorspaces
      , resourceFonts
      , resourceXObjects
      , resourceExtGStates
      , resourceProperties
      ]

  getKeys :: BS.ByteString -> PDFObject -> Maybe [BS.ByteString]
  getKeys key object = do
    getValueForKey key object >>= \case
      PDFDictionary dict -> Just $ Map.keys dict
      _                  -> Nothing

  getResourceKeys :: BS.ByteString -> PDFObject -> Maybe [BS.ByteString]
  getResourceKeys key object = do
    resources <- getValueForKey "Resources" object
    getValueForKey key resources >>= \case
      PDFDictionary dict -> Just $ Map.keys dict
      _                  -> Nothing

{- |
Given a list of PDF objects, generate the PDF file content.

This function recreates the XRef table in the old format.

An error is signaled in the following cases:

- no numbered objects in the list of PDF objects
- no PDF version in the list of PDF objects
- no trailer in the list of PDF objects
-}
pdfEncode
  :: Logging IO
  => PDFDocument -- ^ A collection of PDF objects (order matters)
  -> FallibleT IO BS.ByteString -- ^ A unified error or a bytestring
pdfEncode objects = do
  _ <- liftIO $ hSetBuffering stderr LineBuffering

  -- Extract objects embedded in object streams
  partition <- explodeDocument objects <&> partitionDocument

  when (   null (ppObjectsWithStream partition)
        && null (ppObjectsWithoutStream partition))
       (throwE EncodeNoIndirectObject)

  let context            = ctx ("encode" :: String)
      withStreamCount    = IM.size (ppObjectsWithStream partition)
      withoutStreamCount = IM.size (ppObjectsWithoutStream partition)

  sayF context $ T.concat [ "Indirect object with stream: "
                          , T.pack (show withStreamCount)
                          ]
  sayF context $ T.concat [ "Indirect object without stream: "
                          , T.pack (show withoutStreamCount)
                          ]

  when (null $ ppHeads partition) (throwE EncodeNoVersion)

  let pdfTrailer = getTrailer partition

  when (pdfTrailer == PDFTrailer PDFNull) (throwE EncodeNoTrailer)
  when (hasKey "Encrypt" pdfTrailer) (throwE EncodeEncrypted)

  let resourceNames    = getAllResourceNames (ppObjectsWithoutStream partition)
      nameTranslations = renameStrings resourceNames
      rObjectsWithStream = fmap (renameResources nameTranslations)
                                (ppObjectsWithStream partition)
      rObjectsWithoutStream = fmap (renameResources nameTranslations)
                                   (ppObjectsWithoutStream partition)
      rPartition = partition { ppObjectsWithStream    = rObjectsWithStream
                             , ppObjectsWithoutStream = rObjectsWithoutStream
                             }
  sayF context $ T.concat [ "Found "
                          , T.pack . show $ Map.size nameTranslations
                          , " resource names"
                          ]

  sayF context "Optimizing PDF"

  oObjectsWithStream    <- pMapM (optimize nameTranslations)
                                 (ppObjectsWithStream rPartition)
  oObjectsWithoutStream <- pMapM (optimize nameTranslations)
                                 (ppObjectsWithoutStream rPartition)

  let oPartition = rPartition { ppObjectsWithStream    = oObjectsWithStream
                              , ppObjectsWithoutStream = oObjectsWithoutStream
                              , ppTrailers             = singleton pdfTrailer
                              }

  sayF context "Last cleaning"
  cleaned <- cleanPartition context oPartition

  sayF context "Calculating last object number"
  let
    lastObjectNumber = max (fst . IM.findMax $ ppObjectsWithStream cleaned)
                           (fst . IM.findMax $ ppObjectsWithoutStream cleaned)

  sayF context "Grouping objects without stream"
  let objectsWithoutStream = fromList $ snd <$> IM.toList (ppObjectsWithoutStream cleaned)
  objectStream <- insert objectsWithoutStream (lastObjectNumber + 1)

  sayF context "Encoding PDF"
  encodedObjStm  <- optimize nameTranslations objectStream >>= encodeObject
  encodedStreams <- pMapM encodeObject (ppObjectsWithStream cleaned)
  let
    encodedAll = IM.insert (lastObjectNumber + 1) encodedObjStm encodedStreams
    body = BS.concat $ eoBinaryData . snd <$> IM.toAscList encodedAll

  let pdfHead = fromPDFObject (PDFVersion "1.7")
      pdfEnd  = fromPDFObject PDFEndOfFile

  sayF context "Optimize XRef stream table"
  xref <- do
    let
      xrefst = xrefStreamTable (lastObjectNumber + 2)
                               (BS.length pdfHead)
                               encodedAll

    optimize nameTranslations xrefst >>= updateXRefStm pdfTrailer

  let
    encodedXRef = fromPDFObject xref
    xRefStmOffset = BS.length pdfHead + BS.length body
    startxref = fromPDFObject (PDFStartXRef xRefStmOffset)

  sayF context "PDF has been optimized!"

  return $ BS.concat [pdfHead, body, encodedXRef, startxref, pdfEnd]
