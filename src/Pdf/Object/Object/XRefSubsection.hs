{-|
This module defines what is a PDF object and functions in relation with the
PDF specification.
-}
module Pdf.Object.Object.XRefSubsection
  ( XRefSubsection(XRefSubsection, xrssStart, xrssCount, xrssEntries)
  ) where

import Data.Kind (Type)

import Pdf.Object.Object.XRefEntry (XRefEntry)

{-|
An XRef table may contain many subsections.

An XRef subsection itself may contain many entries.
-}
type XRefSubsection :: Type
data XRefSubsection = XRefSubsection
  { xrssStart   :: !Int -- ^ Index of the first entry
  , xrssCount   :: !Int -- ^ Entry count
  , xrssEntries :: ![XRefEntry] -- ^ Entries
  }
  deriving stock (Eq, Show)

instance Ord XRefSubsection where
  compare :: XRefSubsection -> XRefSubsection -> Ordering
  compare (XRefSubsection xs xc xe) (XRefSubsection ys yc ye) =
    compare xs ys <> compare xc yc <> compare xe ye
