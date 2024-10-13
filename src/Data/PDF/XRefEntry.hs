module Data.PDF.XRefEntry
  ( inUseEntry
  , freeEntry
  , XRefEntry(XRefEntry, xreOffset, xreGeneration, xreState)
  ) where

import Data.Kind (Type)
import Data.PDF.XRefState (XRefState (FreeEntry, InUseEntry))

-- | Entry in an XRef table (old format).
type XRefEntry :: Type
data XRefEntry = XRefEntry
  { xreOffset     :: !Int -- ^ Offset from the beginning of the PDF file
  , xreGeneration :: !Int -- ^ Generation number (usually 0)
  , xreState      :: !XRefState -- ^ State of the entry
  }
  deriving stock (Eq, Show)

instance Ord XRefEntry where
  compare :: XRefEntry -> XRefEntry -> Ordering
  compare (XRefEntry xo xg xs) (XRefEntry yo yg ys) =
    compare xo yo <> compare xg yg <> compare xs ys

-- | Create an XRef entry with the `InUseEntry` state.
inUseEntry :: Int -> Int -> XRefEntry
inUseEntry offset generation = XRefEntry offset generation InUseEntry

{-|
Create an XRef entry with the `FreeEntry` state.

A free entry state has an offset and a generation number of 0.
-}
freeEntry :: XRefEntry
freeEntry = XRefEntry 0 0 FreeEntry
