{-|
Combinations of filters and encoded bytes.

PDF streams can be encoded by applying one or more filters. In DietPDF, a
`FilterCombination` bundles:

* The filter chain (`fcList`).
* The resulting encoded bytes (`fcBytes`).
* A flag indicating whether this filter chain should replace the existing one
  (`fcReplace`) or be appended.

This is typically used while trying alternative encodings and comparing their
sizes.
-}
module Data.PDF.FilterCombination
  ( FilterCombination (FilterCombination, fcList, fcBytes, fcReplace)
  , mkFCAppend
  , mkFCReplace
  , fcLength
  )
where

import Data.Array (mkArray)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.PDF.Filter (Filter)
import Data.PDF.FilterList (FilterList)

{-|
A candidate encoding for a PDF stream.

The `fcReplace` flag describes how the filter list should be applied relative
to an existing filter list.
-}
type FilterCombination :: Type
data FilterCombination = FilterCombination
  { fcList    :: !FilterList
  , fcBytes   :: !ByteString
  , fcReplace :: !Bool
  }
  deriving stock (Show)

{-|
Returns the size in bytes of the encoded representation.
-}
fcLength :: FilterCombination -> Int
fcLength = BS.length . fcBytes

{-|
Builds a `FilterCombination` whose filters should be appended.

This corresponds to applying the provided filter list on top of an existing
filter chain.
-}
mkFCAppend :: [Filter] -> ByteString -> FilterCombination
mkFCAppend fList bytes = FilterCombination (mkArray fList) bytes False

{-|
Builds a `FilterCombination` whose filters should replace the existing ones.
-}
mkFCReplace :: [Filter] -> ByteString -> FilterCombination
mkFCReplace fList bytes = FilterCombination (mkArray fList) bytes True
