module Util.Sequence (mapMaybe) where

import Data.Sequence (Seq (Empty, (:<|)))

mapMaybe :: (a -> Maybe b) -> Seq a -> Seq b
mapMaybe _anyProcess Empty = Empty
mapMaybe process (item :<| remains) =
  let processedRemains = mapMaybe process remains in
    case process item of
      Nothing -> processedRemains
      Just processedItem  -> processedItem :<| processedRemains
