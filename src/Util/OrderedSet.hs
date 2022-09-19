module Util.OrderedSet
  ( fromMostRecents
  -- , head
  -- , last
  ) where

import           Data.Foldable                  ( Foldable(foldl') )
import qualified Data.Set.Ordered              as OS
{- import           Prelude                 hiding ( head
                                                , last
                                                )
 -}
{-
head :: OS.OSet a -> Maybe a
head set = OS.elemAt set 0
-}

{-
last :: OS.OSet a -> Maybe a
last set = OS.elemAt set (OS.size set - 1)
-}

{- |
Create an ordered set from a foldable type and only keep the last version of
items.
-}
fromMostRecents :: (Foldable t, Ord a) => t a -> OS.OSet a
fromMostRecents = foldl' (OS.>|) OS.empty
