module Data.PDF.XRefState
  ( XRefState(InUseEntry, FreeEntry)
  ) where

import Data.Kind (Type)

-- | State of an entry in an XRef table (old format).
type XRefState :: Type
data XRefState
  = InUseEntry -- ^ Regular entry
  | FreeEntry -- ^ Free entry
  deriving stock (Eq, Show)

instance Ord XRefState where
  compare :: XRefState -> XRefState -> Ordering
  compare InUseEntry InUseEntry = EQ
  compare FreeEntry  FreeEntry  = EQ
  compare InUseEntry FreeEntry  = GT
  compare FreeEntry  InUseEntry = LT
