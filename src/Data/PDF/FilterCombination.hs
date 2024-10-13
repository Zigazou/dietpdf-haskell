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

type FilterCombination :: Type
data FilterCombination = FilterCombination
  { fcList    :: !FilterList
  , fcBytes   :: !ByteString
  , fcReplace :: !Bool
  }
  deriving stock (Show)

fcLength :: FilterCombination -> Int
fcLength = BS.length . fcBytes

mkFCAppend :: [Filter] -> ByteString -> FilterCombination
mkFCAppend fList bytes = FilterCombination (mkArray fList) bytes False

mkFCReplace :: [Filter] -> ByteString -> FilterCombination
mkFCReplace fList bytes = FilterCombination (mkArray fList) bytes True
