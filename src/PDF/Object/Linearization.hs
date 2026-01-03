{-|
Support for linearized PDF documents

This module provides support for handling linearized PDF documents.

Linearized PDFs are structured to allow the first page to be displayed quickly
without downloading or parsing the entire file. The first page objects and
resources are placed at the beginning, followed by a hint stream that describes
the structure of the remaining content.
-}
module PDF.Object.Linearization
  ( Linearization
    ( Linearization
    , lnFileLength
    , lnFirstPageEndOffset
    , lnFirstPageNumber
    , lnFirstPageObjectNumber
    , lnNumberOfPages
    , lnOverflowHintLength
    , lnOverflowHintOffset
    , lnPrimaryHintLength
    , lnPrimaryHintOffset
    , lnVersion
    , lnXRefFirstEntryOffset
    )
  , getLinearization
  ) where

import Control.Monad (msum)

import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.PDF.PDFDocument (PDFDocument, lMap)
import Data.PDF.PDFObject
    ( PDFObject (PDFArray, PDFDictionary, PDFIndirectObject, PDFNumber)
    )
import Data.Sequence qualified as SQ

{-|
Linearization information extracted from a PDF's linearization dictionary.

A linearized PDF contains a special dictionary in the first page object that
specifies offsets and lengths for hint streams and page boundaries. This allows
viewers to display the first page before downloading the entire file.

The hints describe which objects are needed for each page and where
cross-reference information can be found.
-}
type Linearization :: Type
data Linearization = Linearization
  { lnVersion               :: !Double
    -- ^ A version identification for the linearized format.
  , lnFileLength            :: !Int
    -- ^ The length of the entire file in bytes. It shall be exactly
    --   equal to the actual length of the PDF file.
  , lnPrimaryHintOffset     :: !Int
    -- ^ offset of the primary hint stream from the beginning of the file.
    --   (This is the beginning of the stream object, not the beginning of
    --   the stream data.)
  , lnPrimaryHintLength     :: !Int
    -- ^ length of the primary hint stream, including stream object overhead.
  , lnOverflowHintOffset    :: !(Maybe Int)
    -- ^ offset of the overflow hint stream from the beginning of the file.
    --   (This is the beginning of the stream object, not the beginning of
    --   the stream data.)
  , lnOverflowHintLength    :: !(Maybe Int)
    -- ^ length of the overflow hint stream, including stream object overhead.
  , lnFirstPageObjectNumber :: !Int
    -- ^ The object number of the first page’s page object.
  , lnFirstPageEndOffset    :: !Int
    -- ^ The offset of the end of the first page, relative to the beginning
    --   of the file.
  , lnNumberOfPages         :: !Int
    -- ^ The number of pages in the document.
  , lnXRefFirstEntryOffset  :: !Int
    -- ^ offset of the white-space character preceding the first entry of the
    --   main cross-reference table (the entry for object number 0), relative
    --   to the beginning of the file. Note that this differs from the Prev
    --   entry in the first-page trailer, which gives the location of the xref
    --   line that precedes the table. Documents that use cross-reference
    --   streams exclusively, this entry shall represent the offset of the main
    --   cross-reference stream object.
  , lnFirstPageNumber       :: !(Maybe Int)
    -- ^ The page number of the first page. Default value: 0.
  }
  deriving stock (Eq, Show)

{-|
Extract a numeric value from a PDF object, if present and numeric.

Returns 'Just' the numeric value if the object is a 'PDFNumber', or 'Nothing'
if the object is absent or of a different type.

__Parameters:__

- Optional PDF object to extract from

__Returns:__ Optional numeric value.
-}
getNumberValue :: Maybe PDFObject -> Maybe Double
getNumberValue (Just (PDFNumber value)) = Just value
getNumberValue _                        = Nothing

{-|
Extract linearization information from a PDF indirect object.

Attempts to extract the linearization dictionary from an indirect object and
parse its entries into a 'Linearization' record. Handles two formats:

1. Without overflow hints: H array with 2 elements [offset, length]
2. With overflow hints: H array with 4 elements [offset, length,
   overflow_offset, overflow_length]

The linearization dictionary entries are keyed by both PDF names and their
abbreviated forms (e.g., "Linearized" or "L").

__Parameters:__

- A PDF indirect object (typically from the first page dictionary)

__Returns:__ 'Just' a 'Linearization' record if successfully parsed, or
'Nothing' if the object is not an indirect object with a linearization
dictionary or if parsing fails.
-}
extractLinearization :: PDFObject -> Maybe Linearization
extractLinearization (PDFIndirectObject _ _ (PDFDictionary dictionary)) =
  case dictionaryEntries of
    [Just (PDFNumber version), Just (PDFNumber fileLength), Just (PDFArray (PDFNumber primaryOffset SQ.:<| PDFNumber primaryLength SQ.:<| SQ.Empty)), Just (PDFNumber firstPageObjectNumber), Just (PDFNumber firstPageEndOffset), Just (PDFNumber numberOfPages), Just (PDFNumber xrefFirstEntryOffset), firstPageNumber]
      -> Just Linearization
        { lnVersion               = version
        , lnFileLength            = round fileLength
        , lnPrimaryHintOffset     = round primaryOffset
        , lnPrimaryHintLength     = round primaryLength
        , lnOverflowHintOffset    = Nothing
        , lnOverflowHintLength    = Nothing
        , lnFirstPageObjectNumber = round firstPageObjectNumber
        , lnFirstPageEndOffset    = round firstPageEndOffset
        , lnNumberOfPages         = round numberOfPages
        , lnXRefFirstEntryOffset  = round xrefFirstEntryOffset
        , lnFirstPageNumber       = round <$> getNumberValue firstPageNumber
        }
    [Just (PDFNumber version), Just (PDFNumber fileLength), Just (PDFArray (PDFNumber primaryOffset SQ.:<| PDFNumber primaryLength SQ.:<| PDFNumber overflowOffset SQ.:<| PDFNumber overflowLength SQ.:<| SQ.Empty)), Just (PDFNumber firstPageObjectNumber), Just (PDFNumber firstPageEndOffset), Just (PDFNumber numberOfPages), Just (PDFNumber xrefFirstEntryOffset), firstPageNumber]
      -> Just Linearization
        { lnVersion               = version
        , lnFileLength            = round fileLength
        , lnPrimaryHintOffset     = round primaryOffset
        , lnPrimaryHintLength     = round primaryLength
        , lnOverflowHintOffset    = Just $ round overflowOffset
        , lnOverflowHintLength    = Just $ round overflowLength
        , lnFirstPageObjectNumber = round firstPageObjectNumber
        , lnFirstPageEndOffset    = round firstPageEndOffset
        , lnNumberOfPages         = round numberOfPages
        , lnXRefFirstEntryOffset  = round xrefFirstEntryOffset
        , lnFirstPageNumber       = round <$> getNumberValue firstPageNumber
        }
    _anyOtherValue -> Nothing
 where
  dictionaryEntries =
    (Map.!?) dictionary <$> ["Linearized", "L", "H", "O", "E", "N", "T", "P"]
extractLinearization _ = Nothing

{-|
Extract linearization information from a PDF document.

Searches through the document's objects to find and parse the linearization
dictionary (typically stored in the first page object). Returns the parsed
linearization information if found and valid, or 'Nothing' if the document is
not linearized or the linearization data is malformed.

__Parameters:__

- A PDF document (collection of objects)

__Returns:__ 'Just' the 'Linearization' data if found and valid, or 'Nothing' if
not linearized or parsing fails.
-}
getLinearization
  :: PDFDocument -- ^ The list of `PDFObject`
  -> Maybe Linearization
     -- ^ The `Linearization` object or `Nothing`. `Nothing` may be returned if
     --   there is no linearization entry or  if it is invalid.
getLinearization = msum . lMap extractLinearization
