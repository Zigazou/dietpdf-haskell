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
                                                , optional
                                                , Mod
                                                , CommandFields
                                                )
import           Codec.Compression.Predictor    ( Predictor )

type Codec :: *
data Codec = LZW
           | Deflate
           | NoCompress
           | RLE
           | Zopfli
           | Ascii85
           | Hex
           deriving stock (Eq, Read, Show)

codecsHelp :: String
codecsHelp =
  "Codec to use (LZW, Deflate, NoCompress, RLE, Zopfli, Ascii85, Hex)"

predictorsHelp :: String
predictorsHelp =
  "Predictor to use (TIFFNoPrediction, TIFFPredictor2, PNGNone, PNGSub, PNGUp, \
  \PNGAverage, PNGPaeth, PNGOptimum)"

type AppOptions :: *
data AppOptions
  = OptimizeOptions !FilePath !FilePath
  | InfoOptions !FilePath
  | ExtractOptions !Int !FilePath
  | HashOptions !FilePath
  | EncodeOptions !Codec !(Maybe FilePath)
  | DecodeOptions !Codec !(Maybe FilePath)
  | PredictOptions !Predictor !Int !Int !(Maybe FilePath)
  | UnpredictOptions !Predictor !Int !Int !(Maybe FilePath)

commandInfo :: Mod CommandFields AppOptions
commandInfo = command
  "info"
  (info
    (InfoOptions <$> argument str (metavar "IN" <> help "PDF file to analyze"))
    (progDesc "Print information about a PDF file")
  )

commandExtract :: Mod CommandFields AppOptions
commandExtract = command
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

commandOptimize :: Mod CommandFields AppOptions
commandOptimize = command
  "optimize"
  (info
    (   OptimizeOptions
    <$> argument str (metavar "IN" <> help "PDF file to process")
    <*> argument str (metavar "OUT" <> help "PDF file to create")
    )
    (progDesc "Optimize a PDF file")
  )
commandHash :: Mod CommandFields AppOptions
commandHash = command
  "hash"
  (info
    (HashOptions <$> argument str (metavar "IN" <> help "PDF file to process"))
    (progDesc "Hash of each stream in a PDF file")
  )

commandEncode :: Mod CommandFields AppOptions
commandEncode = command
  "encode"
  (info
    (   EncodeOptions
    <$> argument auto (metavar "CODEC" <> help codecsHelp)
    <*> optional (argument str (metavar "IN" <> help "File to encode"))
    )
    (progDesc "Encode a file as it would be in a stream")
  )

commandDecode :: Mod CommandFields AppOptions
commandDecode = command
  "decode"
  (info
    (   DecodeOptions
    <$> argument auto (metavar "CODEC" <> help codecsHelp)
    <*> optional (argument str (metavar "OUT" <> help "File to decode"))
    )
    (progDesc "Decode a file as it would be in a stream")
  )

commandPredict :: Mod CommandFields AppOptions
commandPredict = command
  "predict"
  (info
    (   PredictOptions
    <$> argument auto (metavar "PREDICTOR" <> help predictorsHelp)
    <*> argument auto (metavar "COLUMNS" <> help "Width in pixels")
    <*> argument auto (metavar "COMPONENTS" <> help "Number of components")
    <*> optional (argument str (metavar "IN" <> help "File to predict"))
    )
    (progDesc "Predict a file as it would be in a stream")
  )

commandUnpredict :: Mod CommandFields AppOptions
commandUnpredict = command
  "unpredict"
  (info
    (   UnpredictOptions
    <$> argument auto (metavar "PREDICTOR" <> help predictorsHelp)
    <*> argument auto (metavar "COLUMNS" <> help "Width in pixels")
    <*> argument auto (metavar "COMPONENTS" <> help "Number of components")
    <*> optional (argument str (metavar "IN" <> help "File to unpredict"))
    )
    (progDesc "Unpredict a file as it would be in a stream")
  )

appOptions :: Parser AppOptions
appOptions =
  subparser
    $  commandInfo
    <> commandExtract
    <> commandOptimize
    <> commandHash
    <> commandEncode
    <> commandDecode
    <> commandPredict
    <> commandUnpredict
