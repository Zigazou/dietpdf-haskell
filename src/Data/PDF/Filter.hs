{-|
This module contains functions facilitating container manipulation (`PDFArray`,
`PDFDictionary` and `PDFIndirectObject`).
-}
module Data.PDF.Filter
  ( Filter(Filter, fDecodeParms, fFilter)
  , hasNoDecodeParms
  ) where

import Data.Kind (Type)
import Data.PDF.PDFObject (PDFObject (PDFNull))

{-|
A filter with its parameters.
-}
type Filter :: Type
data Filter = Filter
  { fFilter      :: !PDFObject
  , fDecodeParms :: !PDFObject
  }
  deriving stock Show

{-|
Checks whether a `Filter` has no decode parameters (i.e., its `fDecodeParms`
is `PDFNull`).
-}
hasNoDecodeParms :: Filter -> Bool
hasNoDecodeParms = (== PDFNull) . fDecodeParms
