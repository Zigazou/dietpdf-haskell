module Data.ObjectInfo
  ( ObjectInfo(..)
  , StreamInfo(..)
  ) where

import Data.Kind (Type)
import Data.ObjectCategory (ObjectCategory)
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
  } deriving stock (Show)
