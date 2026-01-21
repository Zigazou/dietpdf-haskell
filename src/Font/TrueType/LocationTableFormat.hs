module Font.TrueType.LocationTableFormat
  ( LocationTableFormat(ShortFormat, LongFormat)
  , fromLocationTableFormat
  ) where

import Data.Int (Int16)
import Data.Kind (Type)

{-|
Format of the loca table, determined by the indexToLocFormat field in the head
table.
-}
type LocationTableFormat :: Type
data LocationTableFormat
  = ShortFormat  -- ^ 0: Short offsets (Offset16), actual offset = value * 2
  | LongFormat   -- ^ 1: Long offsets (Offset32), actual offset = value
  deriving stock (Eq, Enum, Show)

fromLocationTableFormat :: LocationTableFormat -> Int16
fromLocationTableFormat ShortFormat = 0
fromLocationTableFormat LongFormat  = 1
