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
import           Control.Monad.State            ( get
                                                , lift
                                                )
import qualified Data.ByteString               as BS
import           Data.Foldable                  ( minimumBy )
import qualified Data.Map.Strict               as Map
import           Pdf.Object.Container           ( Filter(Filter)
                                                , deepMap
                                                , setFilters
                                                , FilterList
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
                                                )
import           Pdf.Object.State               ( FallibleComputation
                                                , getStream
                                                , hasStreamS
                                                , getValue
                                                , setStream
                                                , updateE
                                                , ifObject
                                                )
import           Pdf.Object.String              ( optimizeString )
import           Pdf.Object.Unfilter            ( unfilter )
import           Util.Errors                    ( UnifiedError
                                                  ( InvalidFilterParm
                                                  )
                                                )
import           Util.Array                     ( mkArray
                                                , mkEmptyArray
                                                )
import           Pdf.Graphics.Parser.Stream     ( gfxParse )
import           Pdf.Graphics.Object            ( separateGfx )

eMinOrder
  :: (a, Either b BS.ByteString) -> (a, Either b BS.ByteString) -> Ordering
eMinOrder (_, Right x) (_, Right y) = compare (BS.length x) (BS.length y)
eMinOrder (_, Right _) (_, Left _ ) = LT
eMinOrder (_, Left _ ) (_, Right _) = GT
eMinOrder _            _            = EQ

zopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> (FilterList, Either UnifiedError BS.ByteString)
zopfli _ stream =
  (mkArray [Filter (PDFName "FlateDecode") PDFNull], FL.compress stream)

rle
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> (FilterList, Either UnifiedError BS.ByteString)
rle _ stream =
  (mkArray [Filter (PDFName "RunLengthDecode") PDFNull], RL.compress stream)

rleZopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> (FilterList, Either UnifiedError BS.ByteString)
rleZopfli _ stream =
  ( mkArray
    [ Filter (PDFName "FlateDecode")     PDFNull
    , Filter (PDFName "RunLengthDecode") PDFNull
    ]
  , snd (rle Nothing stream) >>= FL.compress
  )

predZopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> (FilterList, Either UnifiedError BS.ByteString)
predZopfli (Just (width, components)) stream =
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
  , predict PNGUp width components stream >>= FL.compress
  )
predZopfli _noWidth _stream = (mkEmptyArray, Left InvalidFilterParm)

predRleZopfli
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> (FilterList, Either UnifiedError BS.ByteString)
predRleZopfli (Just (width, components)) stream =
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
  , predict PNGSub width components stream >>= FL.compress
  )
predRleZopfli _noWidth _stream = (mkEmptyArray, Left InvalidFilterParm)

applyEveryFilter
  :: Maybe (Int, Int)
  -> BS.ByteString
  -> [(FilterList, Either UnifiedError BS.ByteString)]
applyEveryFilter widthComponents stream =
  [zopfli, rle, predRleZopfli, rleZopfli, predZopfli]
    <*> pure widthComponents
    <*> pure stream

getWidthComponents :: FallibleComputation (Maybe (Int, Int))
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

filterOptimize :: FallibleComputation ()
filterOptimize = ifObject hasStreamS $ do
  stream          <- getStream
  widthComponents <- getWidthComponents
  let (bestFilters, bestStream) = minimumBy
        eMinOrder
        (mkArray (applyEveryFilter widthComponents stream))
  setStream =<< lift bestStream
  setFilters bestFilters

streamIsXML :: PDFObject -> Bool
streamIsXML (PDFIndirectObjectWithStream _ _ dictionary _) =
  dictionary Map.!? "Subtype" == Just (PDFName "XML")
streamIsXML _ = False

streamOptimize :: FallibleComputation ()
streamOptimize = do
  object <- get
  stream <- getStream
  if streamIsXML object
    then setStream (optimizeXML stream)
    else case gfxParse stream of
      Right objects -> setStream (separateGfx objects)
      _error        -> return ()

{- |
Completely refilter a stream by finding the best filter combination.

It also optimized nested strings and XML streams.
-}
refilter :: FallibleComputation ()
refilter = do
  deepMap optimizeString
  ifObject hasStreamS $ do
    deepMap optimizeString
    unfilter
    streamOptimize
    filterOptimize

{- |
Determine if a `PDFObject` is optimizable, whether because its filters are
known by DietPDF or because its structure is optimizable.
-}
optimizable :: PDFObject -> Bool
optimizable (PDFIndirectObjectWithStream _ _ dictionary _) =
  case Map.lookup "Filter" dictionary of
    Just (PDFName "FlateDecode") -> True
    Just (PDFName "RLEDecode"  ) -> True
    Just (PDFName "LZWDecode"  ) -> True
    Nothing                      -> True
    _anyOtherFilter              -> False
optimizable (PDFObjectStream _ _ dictionary _) =
  case Map.lookup "Filter" dictionary of
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
