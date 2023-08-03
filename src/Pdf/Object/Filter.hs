{-|
This modules implements several optimization techniques targeted at PDF objects.
-}
module Pdf.Object.Filter
  ( filterOptimize
  ) where

import qualified Data.ByteString               as BS
import           Data.Foldable                  ( minimumBy )
import           Pdf.Object.Container           ( setFilters
                                                , FilterList
                                                )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFName
                                                  , PDFNumber
                                                  , PDFNumber
                                                  )
                                                , hasStream
                                                )
import           Pdf.Object.State               ( getStream
                                                , getValue
                                                , setStream
                                                )
import           Util.UnifiedError              ( FallibleT )
import           Util.Logging                   ( Logging
                                                , sayComparisonF
                                                )
import           Util.Array                     ( mkArray )
import           Util.ByteString                ( sndLengthCompare )
import qualified Data.Text                     as T
import           Control.Monad.Trans.Except     ( except )
import           Data.Functor                   ( (<&>) )

import           Pdf.Object.FilterCombine.Zopfli
                                                ( zopfli )
import           Pdf.Object.FilterCombine.Rle   ( rle )
import           Pdf.Object.FilterCombine.RleZopfli
                                                ( rleZopfli )
import           Pdf.Object.FilterCombine.PredZopfli
                                                ( predZopfli )
import           Pdf.Object.FilterCombine.PredRleZopfli
                                                ( predRleZopfli )

filterInfo
  :: Logging m => T.Text -> BS.ByteString -> BS.ByteString -> FallibleT m ()
filterInfo filterName streamBefore streamAfter = sayComparisonF
  ("Filter " <> filterName)
  (BS.length streamBefore)
  (BS.length streamAfter)

applyEveryFilter
  :: Logging m
  => Maybe (Int, Int)
  -> BS.ByteString
  -> FallibleT m [(FilterList, BS.ByteString)]
-- The stream has width and components information.
applyEveryFilter widthComponents@(Just (_width, _components)) stream = do
  rRle <- except $ rle widthComponents stream
  filterInfo "RLE" stream (snd rRle)

  rZopfli <- except $ zopfli widthComponents stream
  filterInfo "Zopfli" stream (snd rZopfli)

  if (BS.length . snd $ rRle) < BS.length stream
    then do
      rPredRleZopfli <- except $ predRleZopfli widthComponents stream
      filterInfo "Predictor/Store+RLE+Zopfli" stream (snd rPredRleZopfli)

      rRleZopfli <- except $ rleZopfli widthComponents stream
      filterInfo "RLE+Zopfli" stream (snd rRleZopfli)

      rPredZopfli <- except $ predZopfli widthComponents stream
      filterInfo "Predictor/Zopfli" stream (snd rPredZopfli)

      return [rRle, rZopfli, rPredRleZopfli, rRleZopfli, rPredZopfli]
    else do
      rPredZopfli <- except $ predZopfli widthComponents stream
      filterInfo "Predictor/Zopfli" stream (snd rPredZopfli)

      rPredRleZopfli <- except $ predRleZopfli widthComponents stream
      filterInfo "Predictor/Store+RLE+Zopfli" stream (snd rPredRleZopfli)

      return [rZopfli, rPredZopfli, rPredRleZopfli]

-- The stream has no width nor components information.
applyEveryFilter Nothing stream = do
  rRle <- except $ rle Nothing stream
  filterInfo "RLE" stream (snd rRle)

  rZopfli <- except $ zopfli Nothing stream
  filterInfo "Zopfli" stream (snd rZopfli)

  if (BS.length . snd $ rRle) < BS.length stream
    then do
      rRleZopfli <- except $ rleZopfli Nothing stream
      filterInfo "RLE+Zopfli" stream (snd rRleZopfli)

      return
        [ rRle
        , rZopfli
        , rRleZopfli{-, rDeflate-}
        ]
    else return [rZopfli{-, rDeflate -}
                        ]

getWidthComponents :: Logging m => PDFObject -> FallibleT m (Maybe (Int, Int))
getWidthComponents object = do
  width      <- getValue "Width" object
  colorSpace <- getValue "ColorSpace" object

  let components :: Int
      components = case colorSpace of
        Just (PDFName "DeviceRGB" ) -> 3
        Just (PDFName "DeviceCMYK") -> 4
        _anyOtherValue              -> 1

  case width of
    Just (PDFNumber width') -> return $ Just (round width', components)
    _anyOtherValue          -> return Nothing

filterOptimize :: Logging m => PDFObject -> FallibleT m PDFObject
filterOptimize object = if hasStream object
  then do
    stream          <- getStream object
    widthComponents <- getWidthComponents object

    candidates      <- applyEveryFilter widthComponents stream <&> mkArray
    let (bestFilters, bestStream) = minimumBy sndLengthCompare candidates

    setStream bestStream object >>= setFilters bestFilters
  else return object
