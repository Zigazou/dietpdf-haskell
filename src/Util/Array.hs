{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

{-|
This module defines what is an array at the lowest level.
-}
module Util.Array
  ( Array
  , mkArray
  , mkEmptyArray
  ) where

import qualified Data.Sequence                 as SQ

{-|
An `Array` is a `Sequence` of object.
-}
type Array a = SQ.Seq a

{- |
Create an empty `Array`.
-}
mkEmptyArray :: Array a
mkEmptyArray = SQ.empty

{- |
Create a `PDFArray` from a list of `PDFObject`.
-}
mkArray :: [a] -> Array a
mkArray = SQ.fromList
