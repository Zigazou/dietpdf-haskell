{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{-|
This modules implements several optimization techniques targeted at PDF objects.
-}
module Pdf.Object.Optimize
  ( optimize
  ) where

import qualified Codec.Compression.Flate       as FL
import           Codec.Compression.Predictor    ( Predictor(PNGSub, PNGUp)
                                                , predict
                                                , toWord8
                                                )
import qualified Codec.Compression.RunLength   as RL
import           Codec.Compression.XML          ( optimizeXML )
import           Control.Monad                  ( when )
import           Control.Monad.State            ( StateT
                                                , get
                                                , lift
                                                )
import qualified Data.ByteString               as BS
import           Data.Foldable                  ( minimumBy )
import qualified Data.HashMap.Strict           as HM
import qualified Data.Sequence                 as SQ
import           Pdf.Object.Container           ( Filter(Filter)
                                                , deepMap
                                                , setFilters
                                                )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFDictionary
                                                  , PDFIndirectObject
                                                  , PDFIndirectObjectWithStream
                                                  , PDFName
                                                  , PDFNull
                                                  , PDFNumber
                                                  , PDFObjectStream
                                                  , PDFTrailer
                                                  )
                                                , getStream
                                                , hasStream
                                                , getValue
                                                , setStream
                                                , updateE
                                                )
import           Pdf.Object.String              ( optimizeString )
import           Pdf.Object.Unfilter            ( unfilter )
import           Util.Errors                    ( UnifiedError
                                                  ( InvalidFilterParm
                                                  )
                                                )

eMinOrder
  :: (a, Either b BS.ByteString) -> (a, Either b BS.ByteString) -> Ordering
eMinOrder (_, Right x) (_, Right y) = compare (BS.length x) (BS.length y)
eMinOrder (_, Right _) (_, Left _ ) = LT
eMinOrder (_, Left _ ) (_, Right _) = GT
eMinOrder _            _            = EQ

zopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> ([Filter], Either UnifiedError BS.ByteString)
zopfli _ stream =
  ([Filter (PDFName "FlateDecode") PDFNull], FL.compress stream)

rle
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> ([Filter], Either UnifiedError BS.ByteString)
rle _ stream =
  ([Filter (PDFName "RunLengthDecode") PDFNull], RL.compress stream)

rleZopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> ([Filter], Either UnifiedError BS.ByteString)
rleZopfli _ stream =
  ( [ Filter (PDFName "FlateDecode")     PDFNull
    , Filter (PDFName "RunLengthDecode") PDFNull
    ]
  , snd (rle Nothing stream) >>= FL.compress
  )

predZopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> ([Filter], Either UnifiedError BS.ByteString)
predZopfli (Just (width, components)) stream =
  ( [ Filter
        (PDFName "FlateDecode")
        (PDFDictionary
          (HM.fromList
            [ ("Predictor", PDFNumber (fromIntegral . toWord8 $ PNGUp))
            , ("Columns"  , PDFNumber (fromIntegral width))
            , ("Colors"   , PDFNumber (fromIntegral components))
            ]
          )
        )
    ]
  , predict PNGUp width components stream >>= FL.compress
  )
predZopfli _noWidth _stream = ([], Left InvalidFilterParm)

predRleZopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> ([Filter], Either UnifiedError BS.ByteString)
predRleZopfli (Just (width, components)) stream =
  ( [ Filter
        (PDFName "FlateDecode")
        (PDFDictionary
          (HM.fromList
            [ ("Predictor", PDFNumber (fromIntegral . toWord8 $ PNGSub))
            , ("Columns"  , PDFNumber (fromIntegral width))
            , ("Colors"   , PDFNumber (fromIntegral components))
            ]
          )
        )
    ]
  , predict PNGSub width components stream >>= FL.compress
  )
predRleZopfli _noWidth _stream = ([], Left InvalidFilterParm)

applyEveryFilter
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> [([Filter], Either UnifiedError BS.ByteString)]
applyEveryFilter widthComponents stream =
  [zopfli, rle, predRleZopfli, rleZopfli, predZopfli]
    <*> pure widthComponents
    <*> pure stream

getWidthComponents :: StateT PDFObject (Either UnifiedError) (Maybe (Int, Int))
getWidthComponents = do
  width      <- getValue "Width"
  colorSpace <- getValue "ColorSpace"

  let components :: Int
      components = case colorSpace of
        Just (PDFName "DeviceRGB" ) -> 3
        Just (PDFName "DeviceCMYK") -> 4
        _anyOtherValue              -> 1

  case width of
    Just (PDFNumber width') -> return $ Just (round width', components)
    _anyOtherValue          -> return Nothing

filterOptimize :: StateT PDFObject (Either UnifiedError) ()
filterOptimize = do
  object <- get
  when (hasStream object) $ do
    stream          <- getStream
    widthComponents <- getWidthComponents
    let (bestFilters, bestStream) = minimumBy
          eMinOrder
          (SQ.fromList (applyEveryFilter widthComponents stream))
    setStream =<< lift bestStream
    setFilters bestFilters

streamIsXML :: PDFObject -> Bool
streamIsXML (PDFIndirectObjectWithStream _ _ dictionary _) =
  case dictionary HM.!? "Subtype" of
    Just (PDFName "XML") -> True
    _anyOtherValue       -> False
streamIsXML _ = False

streamOptimize :: StateT PDFObject (Either UnifiedError) ()
streamOptimize = do
  object <- get
  stream <- getStream
  when (streamIsXML object) $ setStream (optimizeXML stream)

{- |
Completely refilter a stream by finding the best filter combination.

It also optimized nested strings and XML streams.
-}
refilter :: StateT PDFObject (Either UnifiedError) ()
refilter = do
  object <- get
  if hasStream object
    then do
      deepMap optimizeString
      unfilter
      streamOptimize
      filterOptimize
    else deepMap optimizeString

{- |
Determine if a `PDFObject` is optimizable, whether because its filters are
known by DietPDF or because its structure is optimizable.
-}
optimizable :: PDFObject -> Bool
optimizable (PDFIndirectObjectWithStream _ _ dictionary _) =
  case HM.lookup "Filter" dictionary of
    Just (PDFName "FlateDecode") -> True
    Just (PDFName "RLEDecode"  ) -> True
    Just (PDFName "LZWDecode"  ) -> True
    Nothing                      -> True
    _anyOtherFilter              -> False
optimizable (PDFObjectStream _ _ dictionary _) =
  case HM.lookup "Filter" dictionary of
    Just (PDFName "FlateDecode") -> True
    Just (PDFName "RLEDecode"  ) -> True
    Just (PDFName "LZWDecode"  ) -> True
    Nothing                      -> True
    _anyOtherFilter              -> False
optimizable PDFIndirectObject{} = True
optimizable PDFTrailer{}        = True
optimizable _                   = False

{- |
Optimize a PDF object.

`PDFObject` may be optimized by:

- using Zopfli instead of Zlib
- combining Zopfli and RLE
- removing unneeded spaces in XML stream

Optimization of spaces is done at the `PDFObject` level, not by this function.

If the PDF object is not elligible to optimization or if optimization is
ineffective, it is returned as is.
-}
optimize :: PDFObject -> PDFObject
optimize object = if optimizable object
  then case updateE object refilter of
    Right optimizedObject -> optimizedObject
    Left  _               -> object
  else object
