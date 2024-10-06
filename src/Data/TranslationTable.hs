module Data.TranslationTable
  ( TranslationTable
  , mkTranslationTable
  , convert
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Map (Map, fromList)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

{- |
A translation table is a map from values to values.
-}
type TranslationTable :: Type
type TranslationTable = Map ByteString ByteString

{- |
Create a translation table from a list of pairs.
-}
mkTranslationTable :: [(ByteString, ByteString)] -> TranslationTable
mkTranslationTable = fromList

{- |
Convert a value using a translation table.

If the value is not in the table, it returns the original value.
-}
convert :: TranslationTable -> ByteString -> ByteString
convert table value = fromMaybe value (Map.lookup value table)
