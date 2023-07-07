{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
This modules implements several optimization techniques targeted at PDF objects.
-}
module Pdf.Object.Filter
  ( filterOptimize
  ) where

import qualified Codec.Compression.Flate       as FL
import           Codec.Compression.Predictor    ( Predictor(PNGSub, PNGUp)
                                                , predict
                                                , toWord8
                                                )
import qualified Codec.Compression.RunLength   as RL
import qualified Data.ByteString               as BS
import           Data.Foldable                  ( minimumBy )
import qualified Data.Map.Strict               as Map
import           Pdf.Object.Container           ( Filter(Filter)
                                                , setFilters
                                                , FilterList
                                                )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFDictionary
                                                  , PDFName
                                                  , PDFNumber
                                                  , PDFNull
                                                  , PDFNull
                                                  , PDFNumber
                                                  )
                                                , hasStream
                                                )
import           Pdf.Object.State               ( getStream
                                                , getValue
                                                , setStream
                                                )
import           Util.UnifiedError              ( UnifiedError
                                                  ( InvalidFilterParm
                                                  )
                                                , FallibleT
                                                )
import           Util.Logging                   ( Logging
                                                , sayComparisonF
                                                )
import           Util.Array                     ( mkArray )
import qualified Data.Text                     as T
import           Control.Monad.Trans.Except     ( except )
import           Data.Functor                   ( (<&>) )

zopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError (FilterList, BS.ByteString)
zopfli _ stream = do
  compressed <- FL.compress stream
  return (mkArray [Filter (PDFName "FlateDecode") PDFNull], compressed)

rle
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError (FilterList, BS.ByteString)
rle _ stream = do
  compressed <- RL.compress stream
  return (mkArray [Filter (PDFName "RunLengthDecode") PDFNull], compressed)

rleZopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError (FilterList, BS.ByteString)
rleZopfli _ stream = do
  compressed <- rle Nothing stream >>= FL.compress . snd
  return
    ( mkArray
      [ Filter (PDFName "FlateDecode")     PDFNull
      , Filter (PDFName "RunLengthDecode") PDFNull
      ]
    , compressed
    )

predZopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError (FilterList, BS.ByteString)
predZopfli (Just (width, components)) stream = do
  compressed <- predict PNGUp width components stream >>= FL.compress
  return
    ( mkArray
      [ Filter
          (PDFName "FlateDecode")
          (PDFDictionary
            (Map.fromList
              [ ("Predictor", PDFNumber (fromIntegral . toWord8 $ PNGUp))
              , ("Columns"  , PDFNumber (fromIntegral width))
              , ("Colors"   , PDFNumber (fromIntegral components))
              ]
            )
          )
      ]
    , compressed
    )
predZopfli _noWidth _stream = Left InvalidFilterParm

predRleZopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> Either UnifiedError (FilterList, BS.ByteString)
predRleZopfli (Just (width, components)) stream = do
  predicted <- predict PNGSub width components stream >>= FL.compress
  return
    ( mkArray
      [ Filter
          (PDFName "FlateDecode")
          (PDFDictionary
            (Map.fromList
              [ ("Predictor", PDFNumber (fromIntegral . toWord8 $ PNGSub))
              , ("Columns"  , PDFNumber (fromIntegral width))
              , ("Colors"   , PDFNumber (fromIntegral components))
              ]
            )
          )
      ]
    , predicted
    )
predRleZopfli _noWidth _stream = Left InvalidFilterParm

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
applyEveryFilter widthComponents@(Just (_width, _components)) stream = do
  rRle <- except $ rle widthComponents stream
  filterInfo "RLE" stream (snd rRle)

  rZopfli <- except $ zopfli widthComponents stream
  filterInfo "Zopfli" stream (snd rZopfli)

  if (BS.length . snd $ rRle) < BS.length stream
    then do
      rPredRleZopfli <- except $ predRleZopfli widthComponents stream
      filterInfo "Predictor+RLE+Zopfli" stream (snd rPredRleZopfli)

      rRleZopfli <- except $ rleZopfli widthComponents stream
      filterInfo "RLE+Zopfli" stream (snd rRleZopfli)

      rPredZopfli <- except $ predZopfli widthComponents stream
      filterInfo "Predictor+Zopfli" stream (snd rPredZopfli)

      return [rRle, rZopfli, rPredRleZopfli, rRleZopfli, rPredZopfli]
    else do
      rPredZopfli <- except $ predZopfli widthComponents stream
      filterInfo "Predictor+Zopfli" stream (snd rPredZopfli)

      return [rZopfli, rPredZopfli]

applyEveryFilter Nothing stream = do
  rRle <- except $ rle Nothing stream
  filterInfo "RLE" stream (snd rRle)

  rZopfli <- except $ zopfli Nothing stream
  filterInfo "Zopfli" stream (snd rZopfli)

  if (BS.length . snd $ rRle) < BS.length stream
    then do
      rRleZopfli <- except $ rleZopfli Nothing stream
      filterInfo "RLE+Zopfli" stream (snd rRleZopfli)

      return [rRle, rZopfli, rRleZopfli]
    else return [rZopfli]

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
    let (bestFilters, bestStream) = minimumBy eMinOrder candidates

    setStream bestStream object >>= setFilters bestFilters
  else return object
 where
  eMinOrder :: (a, BS.ByteString) -> (a, BS.ByteString) -> Ordering
  eMinOrder (_, x) (_, y) = compare (BS.length x) (BS.length y)
