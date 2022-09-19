{-# LANGUAGE OverloadedStrings   #-}

-- | This modules contains functions to help manipulating of ByteString.
module Util.ByteString
  ( splitRaw
  ) where

import qualified Data.ByteString               as BS

{-|
Split a `ByteString` in `ByteString` of specific length.
-}
splitRaw :: Int -> BS.ByteString -> [BS.ByteString]
splitRaw width = splitRaw'
 where
  splitRaw' raw | BS.length chunk == 0 = []
                | otherwise            = chunk : splitRaw' remain
    where (chunk, remain) = BS.splitAt width raw
