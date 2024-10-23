module Data.PDF.ObjectInfo
  ( ObjectInfo(ObjectInfo, oNumber, oDescription, oCategory, oStream, oOffset, oEmbedded)
  , StreamInfo(StreamInfo, sFilteredSize, sUnfilteredSize)
  ) where

import Data.Kind (Type)
import Data.PDF.ObjectCategory (ObjectCategory)
import Data.Text (Text)

type StreamInfo :: Type
data StreamInfo = StreamInfo
  { sFilteredSize   :: !Int
  , sUnfilteredSize :: !Int
  } deriving stock (Show)

type ObjectInfo :: Type
data ObjectInfo = ObjectInfo
  { oNumber      :: !(Maybe Int)
  , oDescription :: !Text
  , oCategory    :: !ObjectCategory
  , oStream      :: !(Maybe StreamInfo)
  , oOffset      :: !(Maybe Int)
  , oEmbedded    :: ![ObjectInfo]
  } deriving stock (Show)
