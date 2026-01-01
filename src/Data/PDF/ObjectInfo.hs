{-|
Metadata extracted about PDF objects.

This module defines simple record types used to report information about objects
found in a PDF.

The intent is descriptive rather than normative: these structures are designed
for statistics/debug output and higher-level summaries.
-}
module Data.PDF.ObjectInfo
  ( ObjectInfo(ObjectInfo, oNumber, oDescription, oCategory, oStream, oOffset, oEmbedded)
  , StreamInfo(StreamInfo, sFilteredSize, sUnfilteredSize)
  ) where

import Data.Kind (Type)
import Data.PDF.ObjectCategory (ObjectCategory)
import Data.Text (Text)

{-|
Size information about an object stream.

Filtered size is the size as stored in the file (after PDF filters are applied).
Unfiltered size is the decoded size.
-}
type StreamInfo :: Type
data StreamInfo = StreamInfo
  { sFilteredSize   :: !Int -- ^ Stream length as stored in the PDF (filtered/encoded).
  , sUnfilteredSize :: !Int -- ^ Stream length after decoding (unfiltered).
  } deriving stock (Show)

{-|
High-level information about a PDF object.

This record is used by reporting and analysis code. Some fields are optional
because the information may not be known (for example when the object does not
have a stable object number in the source).
-}
type ObjectInfo :: Type
data ObjectInfo = ObjectInfo
  { oNumber      :: !(Maybe Int)        -- ^ Object number, when the object originates from an indirect object.
  , oDescription :: !Text               -- ^ Human-readable description used for reports.
  , oCategory    :: !ObjectCategory     -- ^ Coarse-grained category assigned to this object.
  , oStream      :: !(Maybe StreamInfo) -- ^ Stream size information, if this object has a stream.
  , oOffset      :: !(Maybe Int)        -- ^ Byte offset in the input file, when known.
  , oEmbedded    :: ![ObjectInfo]       -- ^ Nested/embedded objects discovered within this object.
  } deriving stock (Show)
