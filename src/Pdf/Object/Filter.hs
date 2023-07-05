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
                                                , sayF
                                                )
import           Util.Array                     ( mkArray )
import qualified Data.Text                     as T
import           Text.Printf                    ( printf )
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

sizeComparison :: BS.ByteString -> BS.ByteString -> T.Text
sizeComparison before after = T.pack
  $ printf "%d/%d (%+.2f%%)" sizeBefore sizeAfter ratio
 where
  sizeBefore = BS.length before
  sizeAfter  = BS.length after
  ratio :: Float
  ratio =
    100
      * (fromIntegral sizeAfter - fromIntegral sizeBefore)
      / fromIntegral sizeBefore

applyEveryFilter
  :: Logging m
  => Maybe (Int, Int)
  -> BS.ByteString
  -> FallibleT m [(FilterList, BS.ByteString)]
applyEveryFilter widthComponents@(Just (_width, _components)) stream = do
  rRle <- except $ rle widthComponents stream
  sayF ("  - Filter RLE                  " <> sizeComparison stream (snd rRle))

  rZopfli <- except $ zopfli widthComponents stream
  sayF
    ("  - Filter Zopfli               " <> sizeComparison stream (snd rZopfli))

  if len rRle < BS.length stream
    then do
      rPredRleZopfli <- except $ predRleZopfli widthComponents stream
      sayF
        (  "  - Filter Predictor+RLE+Zopfli "
        <> sizeComparison stream (snd rPredRleZopfli)
        )

      rRleZopfli <- except $ rleZopfli widthComponents stream
      sayF
        (  "  - Filter RLE+Zopfli           "
        <> sizeComparison stream (snd rRleZopfli)
        )

      rPredZopfli <- except $ predZopfli widthComponents stream
      sayF
        (  "  - Filter Predictor+Zopfli     "
        <> sizeComparison stream (snd rPredZopfli)
        )

      return [rRle, rZopfli, rPredRleZopfli, rRleZopfli, rPredZopfli]
    else do
      rPredZopfli <- except $ predZopfli widthComponents stream
      sayF
        (  "  - Filter Predictor+Zopfli     "
        <> sizeComparison stream (snd rPredZopfli)
        )

      return [rZopfli, rPredZopfli]
 where
  len :: (FilterList, BS.ByteString) -> Int
  len = BS.length . snd

applyEveryFilter Nothing stream = do
  rRle <- except $ rle Nothing stream
  sayF ("  - Filter RLE                  " <> sizeComparison stream (snd rRle))

  rZopfli <- except $ zopfli Nothing stream
  sayF
    ("  - Filter Zopfli               " <> sizeComparison stream (snd rZopfli))

  if len rRle < BS.length stream
    then do
      rRleZopfli <- except $ rleZopfli Nothing stream
      sayF
        (  "  - Filter RLE+Zopfli           "
        <> sizeComparison stream (snd rRleZopfli)
        )

      return [rRle, rZopfli, rRleZopfli]
    else
      return [rZopfli]
 where
  len :: (FilterList, BS.ByteString) -> Int
  len = BS.length . snd

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
