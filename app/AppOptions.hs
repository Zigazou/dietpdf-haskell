{-# LANGUAGE DerivingStrategies #-}
module AppOptions
  ( AppOptions
    ( OptimizeOptions
    , InfoOptions
    , ExtractOptions
    , HashOptions
    , EncodeOptions
    , DecodeOptions
    , PredictOptions
    , UnpredictOptions
    )
  , appOptions
  , Codec(LZW, Deflate, RLE, NoCompress, Zopfli, Ascii85, Hex)
  ) where

import           Options.Applicative            ( Parser
                                                , argument
                                                , command
                                                , help
                                                , info
                                                , metavar
                                                , progDesc
                                                , str
                                                , auto
                                                , subparser
                                                )
import           Codec.Compression.Predictor    ( Predictor )

data Codec = LZW
           | Deflate
           | NoCompress
           | RLE
           | Zopfli
           | Ascii85
           | Hex
           deriving stock (Eq, Read, Show)

data AppOptions
  = OptimizeOptions !FilePath !FilePath
  | InfoOptions !FilePath
  | ExtractOptions !Int !FilePath
  | HashOptions !FilePath
  | EncodeOptions !Codec !FilePath
  | DecodeOptions !Codec !FilePath
  | PredictOptions !Predictor !Int !Int !FilePath
  | UnpredictOptions !Predictor !Int !Int !FilePath

appOptions :: Parser AppOptions
appOptions = subparser
  (  command
      "info"
      (info
        (   InfoOptions
        <$> argument str (metavar "IN" <> help "PDF file to analyze")
        )
        (progDesc "Print information about a PDF file")
      )
  <> command
       "extract"
       (info
         (   ExtractOptions
         <$> argument auto (metavar "NUM" <> help "Object number")
         <*> argument str  (metavar "IN" <> help "PDF file to analyze")
         )
         (progDesc
           "Extract the stream of a specific object from a PDF file \
           \(the stream is unfiltered)"
         )
       )
  <> command
       "optimize"
       (info
         (   OptimizeOptions
         <$> argument str (metavar "IN" <> help "PDF file to process")
         <*> argument str (metavar "OUT" <> help "PDF file to create")
         )
         (progDesc "Optimize a PDF file")
       )
  <> command
       "hash"
       (info
         (   HashOptions
         <$> argument str (metavar "IN" <> help "PDF file to process")
         )
         (progDesc "Hash of each stream in a PDF file")
       )
  <> command
       "encode"
       (info
         (   EncodeOptions
         <$> argument auto (metavar "CODEC" <> help "Codec to use")
         <*> argument str  (metavar "IN" <> help "File to encode")
         )
         (progDesc "Encode a file as it would be in a stream")
       )
  <> command
       "decode"
       (info
         (   DecodeOptions
         <$> argument auto (metavar "CODEC" <> help "Codec to use")
         <*> argument str  (metavar "OUT" <> help "File to decode")
         )
         (progDesc "Decode a file as it would be in a stream")
       )
  <> command
       "predict"
       (info
         (   PredictOptions
         <$> argument auto (metavar "PREDICTOR" <> help "Predictor to use")
         <*> argument auto (metavar "COLUMNS" <> help "Width in pixels")
         <*> argument auto (metavar "COMPONENTS" <> help "Number of components")
         <*> argument str  (metavar "IN" <> help "File to predict")
         )
         (progDesc "Predict a file as it would be in a stream")
       )
  <> command
       "unpredict"
       (info
         (   UnpredictOptions
         <$> argument auto (metavar "PREDICTOR" <> help "Predictor to use")
         <*> argument auto (metavar "COLUMNS" <> help "Width in pixels")
         <*> argument auto (metavar "COMPONENTS" <> help "Number of components")
         <*> argument str  (metavar "IN" <> help "File to unpredict")
         )
         (progDesc "Unpredict a file as it would be in a stream")
       )
  )