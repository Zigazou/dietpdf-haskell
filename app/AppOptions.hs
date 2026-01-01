{- |
Command-line option types and parsers.

This module defines the sum type used to represent command-line options for the
application, along with optparse-applicative parsers for each subcommand.

The main entry point is 'appOptions', which parses an 'AppOptions' value.
-}
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
    , HumanOptions
    , StatOptions
    , GetOptions
    )
  , appOptions
  , Codec(LZW, Deflate, RLE, NoCompress, Zopfli, Ascii85, Hex)
  , FileOverwrite(OverwriteFile, DoNotOverwriteFile)
  ) where

import Codec.Compression.Predict (Predictor)

import Data.Kind (Type)
import Data.PDF.Settings
  ( OptimizeGFX
  , UseGhostScript
  , UsePDFToCairo
  , UseZopfli
  , toOptimizeGFX
  , toUseGhostScript
  , toUsePDFToCairo
  , toUseZopfli
  )

import Options.Applicative
  ( Alternative (some)
  , CommandFields
  , Mod
  , Parser
  , argument
  , auto
  , command
  , help
  , info
  , long
  , metavar
  , optional
  , progDesc
  , short
  , str
  , subparser
  , switch
  )

{-|
Indicates whether to overwrite an existing file or not.
-}
type FileOverwrite :: Type
data FileOverwrite = OverwriteFile
                   | DoNotOverwriteFile
                   deriving stock (Eq, Read, Show)

{-|
Convert a boolean to a `FileOverwrite` value.
-}
toOverwriteFile :: Bool -> FileOverwrite
toOverwriteFile True  = OverwriteFile
toOverwriteFile False = DoNotOverwriteFile

{-|
List of supported codecs.
-}
type Codec :: Type
data Codec = LZW        -- ^ LZW compression (LZWDecode)
           | Deflate    -- ^ Deflate compression (FlateDecode)
           | NoCompress -- ^ No compression
           | RLE        -- ^ Run-Length Encoding (RunLengthDecode)
           | Zopfli     -- ^ Zopfli compression (FlateDecode with Zopfli)
           | Ascii85    -- ^ ASCII85 encoding (ASCII85Decode)
           | Hex        -- ^ ASCIIHex encoding (ASCIIHexDecode)
           deriving stock (Eq, Read, Show)

{-|
Help message for codecs.
-}
codecsHelp :: String
codecsHelp =
  "Codec to use (LZW, Deflate, NoCompress, RLE, Zopfli, Ascii85, Hex)"

{-|
Help message for predictors.
-}
predictorsHelp :: String
predictorsHelp =
  "Predictor to use (TIFFNoPrediction, TIFFPredictor2, PNGNone, PNGSub, PNGUp, \
  \PNGAverage, PNGPaeth, PNGOptimum)"

{-|
Application options.
-}
type AppOptions :: Type
data AppOptions
  = OptimizeOptions !FilePath !(Maybe FilePath) !UseGhostScript !UsePDFToCairo !UseZopfli !OptimizeGFX !FileOverwrite
  | InfoOptions !FilePath
  | ExtractOptions !Int !FilePath
  | HashOptions !FilePath
  | EncodeOptions !Codec !(Maybe FilePath)
  | DecodeOptions !Codec !(Maybe FilePath)
  | PredictOptions !Predictor !Int !Int !(Maybe FilePath)
  | UnpredictOptions !Predictor !Int !Int !(Maybe FilePath)
  | HumanOptions !(Maybe FilePath)
  | StatOptions ![FilePath]
  | GetOptions !Int !FilePath

{-|
Parser for the info command.
-}
commandInfo :: Mod CommandFields AppOptions
commandInfo = command
  "info"
  (info
    (InfoOptions <$> argument str (metavar "input_pdf_file" <> help "PDF file to analyze"))
    (progDesc "Print information about a PDF file")
  )

{-|
Parser for the extract command.
-}
commandExtract :: Mod CommandFields AppOptions
commandExtract = command
  "extract"
  (info
    (   ExtractOptions
    <$> argument auto (metavar "<object_number>" <> help "Object number")
    <*> argument str  (metavar "<input_pdf_file>" <> help "PDF file to analyze")
    )
    (progDesc
      "Extract the stream of a specific object from a PDF file \
           \(the stream is unfiltered)"
    )
  )

{-|
Parser for the optimize command.
-}
commandOptimize :: Mod CommandFields AppOptions
commandOptimize = command
  "optimize"
  (info
    (   OptimizeOptions
    <$> argument str (metavar "<input_pdf_file>" <> help "PDF file to process")
    <*> optional (argument str (metavar "<output_pdf_file>" <> help "PDF file to create"))
    <*> (toUseGhostScript <$> switch (long "gs-optimize" <> short 'g' <> help "Use GhostScript before optimizing"))
    <*> (toUsePDFToCairo <$> switch (long "p2c-optimize" <> short 'p' <> help "Use PDFToCairo before optimizing"))
    <*> (toUseZopfli . not <$> switch (long "no-zopfli" <> short 'z' <> help "Do not use Zopfli"))
    <*> (toOptimizeGFX . not <$> switch (long "no-gfx-optimize" <> short 'x' <> help "Do not optimize graphics stream"))
    <*> (toOverwriteFile <$> switch (long "overwrite" <> short 'o' <> help "Overwrite existing output file"))
    )
    (progDesc "Optimize a PDF file")
  )

{-|
Parser for the hash command.
-}
commandHash :: Mod CommandFields AppOptions
commandHash = command
  "hash"
  (info
    (HashOptions <$> argument str (metavar "<input_pdf_file>" <> help "PDF file to process"))
    (progDesc "Hash of each stream in a PDF file")
  )

{-|
Parser for the encode command.
-}
commandEncode :: Mod CommandFields AppOptions
commandEncode = command
  "encode"
  (info
    (   EncodeOptions
    <$> argument auto (metavar "<codec>" <> help codecsHelp)
    <*> optional (argument str (metavar "[input_pdf_file]" <> help "File to encode"))
    )
    (progDesc "Encode a file as it would be in a stream")
  )

{-|
Parser for the decode command.
-}
commandDecode :: Mod CommandFields AppOptions
commandDecode = command
  "decode"
  (info
    (   DecodeOptions
    <$> argument auto (metavar "<codec>" <> help codecsHelp)
    <*> optional (argument str (metavar "[output_pdf_file]" <> help "File to decode"))
    )
    (progDesc "Decode a file as it would be in a stream")
  )

{-|
Parser for the predict command.
-}
commandPredict :: Mod CommandFields AppOptions
commandPredict = command
  "predict"
  (info
    (   PredictOptions
    <$> argument auto (metavar "<predictor>" <> help predictorsHelp)
    <*> argument auto (metavar "<columns>" <> help "Width in pixels")
    <*> argument auto (metavar "<components>" <> help "Number of components")
    <*> optional (argument str (metavar "<input_pdf_file>" <> help "File to predict"))
    )
    (progDesc "Predict a file as it would be in a stream")
  )

{-|
Parser for the unpredict command.
-}
commandUnpredict :: Mod CommandFields AppOptions
commandUnpredict = command
  "unpredict"
  (info
    (   UnpredictOptions
    <$> argument auto (metavar "<predictor>" <> help predictorsHelp)
    <*> argument auto (metavar "<columns>" <> help "Width in pixels")
    <*> argument auto (metavar "<components>" <> help "Number of components")
    <*> optional (argument str (metavar "<input_pdf_file>" <> help "File to unpredict"))
    )
    (progDesc "Unpredict a file as it would be in a stream")
  )

{-|
Parser for the human command.
-}
commandHuman :: Mod CommandFields AppOptions
commandHuman = command
  "human"
  (info
    (   HumanOptions
    <$> optional (argument str (metavar "<input_gfx_file>" <> help "Graphics code to make human"))
    )
    (progDesc "Print graphics code in a readable human form")
  )

{-|
Parser for the stat command.
-}
commandStat :: Mod CommandFields AppOptions
commandStat = command
  "stat"
  (info
    (StatOptions <$> some (argument str (metavar "<input_pdf_files>" <> help "PDF files to analyze")))
    (progDesc "Print statistics about a PDF file")
  )

{-|
Parser for the get command.
-}
commandGet :: Mod CommandFields AppOptions
commandGet = command
  "get"
  (info
    (   GetOptions
    <$> argument auto (metavar "<object_number>" <> help "Object number")
    <*> argument str  (metavar "<input_pdf_file>" <> help "PDF file to query")
    )
    (progDesc "Get object from a PDF file")
  )

{-|
Parser for application options.
-}
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
    <> commandHuman
    <> commandStat
    <> commandGet
