-- | Mask-specific encoding search. Soft masks remain DeviceGray SMask images,
-- including when reduced to one bit; stencil polarity is preserved separately.
module PDF.Document.OptimizeBitmapMasks (optimizeBitmapMasks) where

import Codec.Compression.CCITTG4 (encodeG4)
import Codec.Compression.ECT qualified as ECT
import Codec.Compression.Flate qualified as Flate
import Codec.Compression.Predict
  (Entropy (EntropyShannon), Predictor (PNGOptimum), predictPNGVariants)

import Control.Monad (forM, forM_, when)
import Control.Monad.State (gets)

import Data.Bitmap.BitmapConfiguration
  (BitmapConfiguration (BitmapConfiguration))
import Data.Bitmap.BitsPerComponent (BitsPerComponent (BC1Bit, BC8Bits))
import Data.Bitmap.MaskAnalysis (defaultMaskThresholds)
import Data.Bitmap.OptimizeMask (maskCandidates, packMask)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Context (Contextual (ctx))
import Data.List (minimumBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Ord (comparing)
import Data.PDF.FilterCombination (FilterCombination (fcBytes, fcList))
import Data.PDF.FilterList (filtersFilter, filtersParms)
import Data.PDF.PDFObject
  ( PDFObject (PDFArray, PDFBool, PDFIndirectObjectWithStream, PDFName, PDFNumber)
  , mkPDFDictionary
  )
import Data.PDF.PDFWork
  (PDFWork, getObject, putObject, sayComparisonP, withContext)
import Data.PDF.Settings
  ( UseCompressor (UseBrotli, UseDeflate, UseECT, UseZopfli)
  , sCompressor
  , sLossyMasks
  )
import Data.PDF.WorkData (WorkData (wSettings))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.UnifiedError (UnifiedError)

import PDF.Document.AnalyzeBitmapMasks
  ( BitmapMaskInfo (bitmapMaskAnalysis, bitmapMaskObject, bitmapMaskRoles)
  , BitmapMaskRole (SoftMask)
  , analyzeBitmapMasks
  , readBitmapMask
  )
import PDF.Object.Object.FromPDFObject (fromPDFObject)
import PDF.Object.Object.Properties (getValueForKey)
import PDF.Object.Object.ToPDFNumber (mkPDFNumber)
import PDF.Processing.ApplyFilter.Helpers (filterInfo, filterInfoCompressor)
import PDF.Processing.FilterCombine.PredRleCompressor
  (predRleCompressorFromPredicted, predRleEntropies)
import PDF.Processing.FilterCombine.RleCompressor (rleCompressor)

-- | Return every detected mask number, including unsupported masks, so the
-- generic image pipeline cannot subsequently apply JPEG or other lossy codecs.
-- Objects are replaced once, after comparing complete serialized sizes.
optimizeBitmapMasks :: PDFWork IO (Set Int)
optimizeBitmapMasks = do
  infos <- analyzeBitmapMasks defaultMaskThresholds
  lossy <- gets (sLossyMasks . wSettings)
  compression <- gets (sCompressor . wSettings)
  let
    -- Keep bitmap masks on standard PDF lossless filters even when
    -- Brotli is requested for other streams.
    selected :: UseCompressor
    selected = case compression of
      UseBrotli -> UseDeflate
      other     -> other

    -- Retain the actual compressor for logging, including fallback.
    compress :: ByteString -> Either UnifiedError (UseCompressor, ByteString)
    compress bytes = case (case selected of
      UseZopfli  -> Flate.compress bytes
      UseECT     -> ECT.compress bytes
      UseDeflate -> Flate.fastCompress bytes
      UseBrotli  -> Flate.fastCompress bytes) of
        Right result -> Right (selected, result)
        Left _unavailableCompressor ->
          (UseDeflate,) <$> Flate.fastCompress bytes

  forM_ infos $ \info -> do
    stored <- getObject (bitmapMaskObject info)
    case (stored, bitmapMaskAnalysis info) of
      (Just original@(PDFIndirectObjectWithStream number revision dict _)
        , Right analysis) -> do
        decoded <- readBitmapMask (bitmapMaskRoles info) original
        case decoded of
          Left _reason -> return ()
          Right (width, height, alpha) -> do
            let
              soft :: Bool
              soft = bitmapMaskRoles info == Set.singleton SoftMask

              -- Matte preblending depends on the original alpha. Nested
              -- masks and alternate images also require more context.
              allowLossy :: Bool
              allowLossy = lossy && soft && all
                (\key -> isNothing (getValueForKey key original))
                ["Matte", "Mask", "SMask", "Alternates"]

              variants :: [(Bool, Int, Int, ByteString)]
              variants = maskCandidates allowLossy analysis width height alpha

              -- Compare candidates by their complete serialized object size.
              size :: PDFObject -> Int
              size = BS.length . fromPDFObject

              -- Build a candidate stream while retaining unrelated original
              -- dictionary entries and updating its geometry and encoding.
              build
                :: Int
                -> Int
                -> Int
                -> [(ByteString, PDFObject)]
                -> ByteString
                -> PDFObject
              build w h bits filters bytes =
                PDFIndirectObjectWithStream number revision
                  (Map.union
                    (Map.fromList
                      ( [ ("Width", mkPDFNumber w)
                        , ("Height", mkPDFNumber h)
                        , ("BitsPerComponent", mkPDFNumber bits)
                        , ("Length", mkPDFNumber (BS.length bytes))
                        ]
                      ++ [ ( "Decode"
                          , PDFArray (Seq.fromList [PDFNumber 1, PDFNumber 0])
                          )
                        | not soft
                        ]
                      ++ [ ( "Interpolate", PDFBool True)
                        | w /= width || h /= height
                        ]
                      ++ filters
                      )
                    )
                    (foldr
                      Map.delete
                      dict
                      [ "Filter"
                      , "DecodeParms"
                      , "Decode"
                      ]
                    )
                  )
                  bytes

            candidates <- forM (zip [1 :: Int ..] variants) $
              \(variant, (isLossy, w, h, samples)) -> do
                let
                  -- Whether every sample can be stored as a one-bit mask.
                  binary :: Bool
                  binary = BS.all (\v -> v == 0 || v == 255) samples

                  bits :: Int
                  bits = if binary then 1 else 8 :: Int

                  -- Packed row-aligned samples for binary masks, or raw alpha.
                  raw :: ByteString
                  raw = if binary then packMask w h samples else samples

                  -- Pixel layout passed to PNG predictor implementations.
                  config :: BitmapConfiguration
                  config = BitmapConfiguration
                            w
                            1
                            (if binary then BC1Bit else BC8Bits)

                  -- Candidate streams with ordinary Flate compression.
                  plain :: [(PDFWork IO (), PDFObject)]
                  plain =
                    [ (filterInfoCompressor used "" raw bytes, flate [] bytes)
                    | Right (used, bytes) <- [compress raw]
                    ]

                  -- Share row predictions across Flate and all RLE strategies.
                  -- Limit Flate search to adaptive prediction to save
                  -- compression attempts; Shannon selection is not a
                  -- compressed-size guarantee.
                  pngPredictors :: [Predictor]
                  pngPredictors = [PNGOptimum]

                  sharedPredictions
                    :: Either UnifiedError ([ByteString], [ByteString])
                  sharedPredictions =
                    splitAt (length pngPredictors)
                      <$> predictPNGVariants
                          (  [ (EntropyShannon, p)
                             | p <- pngPredictors
                             ]
                          ++ [ (entropy, PNGOptimum)
                             | entropy <- predRleEntropies
                             ]
                          )
                          config
                          raw

                  -- Candidate streams with each supported PNG predictor.
                  predicted :: [(PDFWork IO (), PDFObject)]
                  predicted =
                    [ ( filterInfoCompressor
                          used
                          (T.pack (show predictor) <> "+")
                          raw
                          bytes
                      , flate
                          [ ( "DecodeParms"
                            , mkPDFDictionary
                              [ ("Predictor", mkPDFNumber predictor)
                              , ("Columns", mkPDFNumber w)
                              , ("Colors", PDFNumber 1)
                              , ("BitsPerComponent", mkPDFNumber bits)
                              ]
                            )
                          ]
                          bytes
                      )
                    | Right (streams, _) <- [sharedPredictions]
                    , (predictor, stream) <- zip pngPredictors streams
                    , Right (used, bytes) <- [compress stream]
                    ]

                  -- Reuse the generic lossless chains, including the stored
                  -- Flate stage that carries predictor parameters before RLE.
                  combined :: [(PDFWork IO (), PDFObject)]
                  combined =
                    [ ( filterInfoCompressor used label raw (fcBytes result)
                      , build
                        w
                        h
                        bits
                        [ (key, value)
                        | (key, Just value) <-
                          [ ("Filter", filtersFilter (fcList result))
                          , ("DecodeParms", filtersParms (fcList result))
                          ]
                        ]
                        (fcBytes result)
                      )
                    | (label, encode) <-
                        [ ("RLE+", rleCompressor (Just config) raw)
                        , ("PredictorPNG+RLE+", \compressor -> do
                            (_, streams) <- sharedPredictions
                            predRleCompressorFromPredicted config
                                                          streams
                                                          compressor)
                        ]
                    , Right (used, result) <-
                        [ case encode selected of
                            Right encoded
                              -> Right (selected, encoded)
                            Left _unavailableCompressor
                              -> (UseDeflate,) <$> encode UseDeflate
                        ]
                    ]

                  -- Group-4 candidates, available only for binary samples.
                  fax :: [(PDFWork IO (), PDFObject)]
                  fax =
                    [ ( filterInfo "CCITTGroup4" raw bytes
                      , build w h bits
                        [ ("Filter", PDFName "CCITTFaxDecode")
                        , ( "DecodeParms"
                          , mkPDFDictionary
                              [ ("K", PDFNumber (-1))
                              , ("Columns", mkPDFNumber w)
                              , ("Rows", mkPDFNumber h)
                              , ("BlackIs1", PDFBool True)
                              , ("EndOfBlock", PDFBool False)
                              ]
                          )
                        ]
                        bytes
                      )
                    | binary
                    , Right bytes <- [encodeG4 w h raw]
                    ]

                  -- Construct a Flate stream with optional predictor
                  -- parameters.
                  flate :: [(ByteString, PDFObject)] -> ByteString -> PDFObject
                  flate parms = build w h bits
                    (("Filter", PDFName "FlateDecode") : parms)

                let
                  attempts :: [(PDFWork IO (), PDFObject)]
                  attempts = plain ++ predicted ++ combined ++ fax

                withContext (  ctx original
                            <> ctx (  "variant " ++ show variant
                                   ++ if isLossy then " (lossy)"
                                                 else " (original)"
                                   )
                            <> ctx (  show w ++ "x" ++ show h ++ " "
                                   ++ show bits ++ "-bit"
                                   )
                            ) $
                  mapM_ fst attempts

                return (map snd attempts)

            let
              -- Smallest serialized object among the original and candidates.
              best :: PDFObject
              best = minimumBy (comparing size) (original : concat candidates)

            when (size best < size original) $ do
                sayComparisonP "Bitmap mask optimization"
                               (size original)
                               (size best)
                putObject best

      _unsupported -> return ()

  return (Set.fromList (map bitmapMaskObject infos))
