module Pdf.Object.FilterCombine.FilterCombination
  ( FilterCombination (FilterCombination, fcList, fcBytes, fcReplace)
  , mkFCAppend
  , mkFCReplace
  , fcLength
  )
where

import Data.ByteString qualified as BS
import Data.Kind (Type)

import Pdf.Object.Container (Filter, FilterList)

import Util.Array (mkArray)

type FilterCombination :: Type
data FilterCombination = FilterCombination
  { fcList    :: !FilterList
  , fcBytes   :: !BS.ByteString
  , fcReplace :: !Bool
  }
  deriving stock (Show)

fcLength :: FilterCombination -> Int
fcLength = BS.length . fcBytes

mkFCAppend :: [Filter] -> BS.ByteString -> FilterCombination
mkFCAppend fList bytes = FilterCombination (mkArray fList) bytes False

mkFCReplace :: [Filter] -> BS.ByteString -> FilterCombination
mkFCReplace fList bytes = FilterCombination (mkArray fList) bytes True
