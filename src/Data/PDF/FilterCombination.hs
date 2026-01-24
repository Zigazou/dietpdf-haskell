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
  , firstPredictor
  )
where

import Data.Array (mkArray)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.PDF.Filter (Filter(fDecodeParms))
import Data.PDF.FilterList (FilterList)
import Data.Sequence (Seq((:<|)))
import Codec.Compression.Predict (Predictor)
import Data.PDF.PDFObject (PDFObject(PDFNumber, PDFDictionary))
import Codec.Compression.Predict.Predictor (decodePredictor)
import Data.Map qualified as Map

{-|
A candidate encoding for a PDF stream.

The `fcReplace` flag describes how the filter list should be applied relative
to an existing filter list.
-}
type FilterCombination :: Type
data FilterCombination = FilterCombination
  { fcList    :: !FilterList -- ^ List of filters applied
  , fcBytes   :: !ByteString -- ^ Encoded bytes
  , fcReplace :: !Bool -- ^ Whether to replace existing filters (True) or append (False)
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

{-|
Returns the first `Predictor` found in the filter combination, if any.
-}
firstPredictor :: FilterCombination -> Maybe Predictor
firstPredictor fc = case fcList fc of
  (filterCombination :<| _) -> case fDecodeParms filterCombination of
    (PDFDictionary dict) -> dict Map.!? "Predictor" >>= \case
      PDFNumber value -> case decodePredictor . round $ value of
        Right predictor -> return predictor
        _anythingElse  -> Nothing
      _anyOtherObject -> Nothing
    _anyOtherObject -> Nothing
  _anyOtherCase -> Nothing
