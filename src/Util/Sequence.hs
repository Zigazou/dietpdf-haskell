{-|
Small helpers for 'Data.Sequence'.

This module provides utility functions for working with strict sequences
('Data.Sequence.Seq') that are not available in the base API used by this
project.
-}
module Util.Sequence (mapMaybe) where

import Data.Sequence (Seq (Empty, (:<|)))

{-|
Map over a sequence and keep only the successful results.

This is the 'Seq' analogue of @Data.Maybe.mapMaybe@ for lists: each element is
mapped with the provided function, 'Nothing' results are discarded, and 'Just'
results are kept in order.
-}
mapMaybe :: (a -> Maybe b) -> Seq a -> Seq b
mapMaybe _anyProcess Empty = Empty
mapMaybe process (item :<| remains) =
  let processedRemains = mapMaybe process remains in
    case process item of
      Nothing -> processedRemains
      Just processedItem  -> processedItem :<| processedRemains
