{-|
Main module of the application.

Provides the 'main' function to run the application.
-}
module Main
  ( main
  ) where

import AppOptions
  ( AppOptions (DecodeOptions, EncodeOptions, ExtractOptions, GetOptions, HashOptions, HumanOptions, InfoOptions, OptimizeOptions, PredictOptions, StatOptions, UnpredictOptions, VersionOptions)
  , FileOverwrite (DoNotOverwriteFile)
  , appOptions
  )

import Command.Decode (decodeByteString)
import Command.Encode (encodeByteString)
import Command.Extract (extract)
import Command.GetObjectByNumber (getObjectByNumber)
import Command.Hash (objectHashes)
import Command.Human (humanByteString)
import Command.Info (showInfo)
import Command.Optimize (optimize)
import Command.Predict (predictByteString)
import Command.Stat (showStat)
import Command.Unpredict (unpredictByteString)

import Control.Exception (tryJust)
import Control.Monad (guard, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT, throwE)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Context (Contextual (ctx))
import Data.Fallible (FallibleT, tryF)
import Data.Logging (sayComparisonF)
import Data.PDF.PDFDocument (PDFDocument)
import Data.PDF.Settings
  ( Settings (Settings, sOptimizeGFX, sUseGhostScript, sUsePDFToCairo, sZopfli)
  , UseGhostScript (DoNotUseGhostScript, UseGhostScript)
  , UsePDFToCairo (DoNotUsePDFToCairo, UsePDFToCairo)
  )
import Data.UnifiedError
  (UnifiedError (CannotOverwriteFile, ParseError, UnableToOpenFile))

import External.GhostScriptOptimize (ghostScriptOptimize)
import External.PDFToCairoOptimize (pdfToCairoOptimize)

import Hexdump (Cfg, defaultCfg, prettyHexCfg, startByte)

import Options.Applicative
  (ParserInfo, execParser, fullDesc, header, helper, info, progDesc, (<**>))

import PDF.Document.Parser (pdfParse)

import Data.Version (showVersion)

import Paths_dietpdf qualified

import System.FilePath (takeFileName)
import System.IO (hClose)
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (withSystemTempFile)
import System.Posix (fileSize, getFileStatus)

{-|
Read a PDF file and parse it.
-}
readPDF :: FilePath -> FallibleT IO PDFDocument
readPDF filename = do
  lift (tryJust (guard . isDoesNotExistError) (BS.readFile filename)) >>= \case
    Right bytes -> pdfParse bytes
    Left _error -> throwE UnableToOpenFile

{-|
Read a ByteString from a file or from standard input.
-}
readByteString :: Maybe FilePath -> FallibleT IO ByteString
readByteString (Just filename) = do
  lift (tryJust (guard . isDoesNotExistError) (BS.readFile filename)) >>= \case
    Right bytes -> return bytes
    Left _error -> throwE UnableToOpenFile
readByteString Nothing =
  lift (tryJust (guard . isDoesNotExistError) BS.getContents) >>= \case
    Right bytes -> return bytes
    Left _error -> throwE UnableToOpenFile

{-|
Get the size of a file in bytes.
-}
getFileSize :: FilePath -> IO Int
getFileSize path = do
    stat <- getFileStatus path
    return $ fromIntegral (fileSize stat)

{-|
Check whether a file exists.
-}
doesFileExist :: FilePath -> IO Bool
doesFileExist path = do
  tryJust (guard . isDoesNotExistError) (getFileStatus path) >>= \case
    Right _stat -> return True
    Left  _err  -> return False

{-|
Configuration for hexdump starting at a given offset.
-}
hexCfg :: Int -> Cfg
hexCfg offset = defaultCfg { startByte = offset }

{-|
Display a hexdump of a ByteString starting at a given offset.
-}
hexDump :: Int -> ByteString -> IO ()
hexDump offset bytes = do
  let bytes' = BS.take 256 (BS.drop offset bytes)

  putStrLn $ prettyHexCfg (hexCfg offset) bytes'

{-|
Run the application with given options.
-}
runApp :: AppOptions -> FallibleT IO ()
runApp VersionOptions = do
  liftIO $ putStrLn $ "dietpdf version " <> showVersion Paths_dietpdf.version

runApp (InfoOptions inputPDF) = readPDF inputPDF >>= showInfo

runApp (ExtractOptions objectNumber inputPDF) =
  readPDF inputPDF >>= extract objectNumber

runApp (OptimizeOptions inputPDF mOutputPDF useGS usePTC useZopfli optimizeGFX overwriteFile) = do
  let settings = Settings { sOptimizeGFX    = optimizeGFX
                          , sZopfli         = useZopfli
                          , sUseGhostScript = useGS
                          , sUsePDFToCairo  = usePTC
                          }
  case (useGS, usePTC) of
    (UseGhostScript, DoNotUsePDFToCairo) ->
      withSystemTempFile (inputPDF <> ".ghostscript") $ \ghostscriptPDF ghostscriptHandle -> do
        -- Close the handles so external programs can use the files.
        lift $ hClose ghostscriptHandle

        -- Optimize PDF with GhostScript.
        ghostScriptOptimize inputPDF ghostscriptPDF

        -- Compare the sizes of the original and GhostScript PDFs.
        originalSize    <- lift $ getFileSize inputPDF
        ghostScriptSize <- lift $ getFileSize ghostscriptPDF

        sayComparisonF (ctx ("ghostscript" :: String))
                      "GhostScripted PDF" originalSize ghostScriptSize

        go ghostscriptPDF settings overwriteFile

    (DoNotUseGhostScript, UsePDFToCairo) ->
      withSystemTempFile (inputPDF <> ".pdftocairo") $ \pdfToCairoPDF pdfToCairoHandle -> do
        -- Close the handles so external programs can use the files.
        lift $ hClose pdfToCairoHandle

        -- Optimize PDF with PDFToCairo.
        pdfToCairoOptimize inputPDF pdfToCairoPDF

        -- Compare the sizes of the original and PDFToCairo PDFs.
        originalSize   <- lift $ getFileSize inputPDF
        pdfToCairoSize <- lift $ getFileSize pdfToCairoPDF

        sayComparisonF (ctx ("pdftocairo" :: String))
                      "PDFToCairo'd PDF" originalSize pdfToCairoSize

        go pdfToCairoPDF settings overwriteFile

    (UseGhostScript, UsePDFToCairo) ->
      withSystemTempFile (inputPDF <> ".ghostscript") $ \ghostscriptPDF ghostscriptHandle -> do
        withSystemTempFile (inputPDF <> ".pdftocairo") $ \pdfToCairoPDF pdfToCairoHandle -> do
          -- Close the handles so external programs can use the files.
          lift $ hClose ghostscriptHandle
          lift $ hClose pdfToCairoHandle

          -- Optimize PDF with GhostScript and PDFToCairo.
          ghostScriptOptimize inputPDF ghostscriptPDF
          pdfToCairoOptimize ghostscriptPDF pdfToCairoPDF

          -- Compare the sizes of the original and PDFToCairo PDFs.
          originalSize   <- lift $ getFileSize inputPDF
          pdfToCairoSize <- lift $ getFileSize pdfToCairoPDF

          sayComparisonF (ctx ("ghostscript+pdftocairo" :: String))
                        "optimized PDF" originalSize pdfToCairoSize

          go pdfToCairoPDF settings overwriteFile

    (DoNotUseGhostScript, DoNotUsePDFToCairo) -> go inputPDF settings overwriteFile
 where
  go :: FilePath -> Settings -> FileOverwrite -> FallibleT IO ()
  go pdfToOptimize settings overwriteFile' = do
    -- Get output PDF path.
    let outputPDF = case mOutputPDF of
          Just path -> path
          Nothing ->
            case reverse inputPDF of
                c1:c2:c3:'.':rest | (c1 == 'f' || c1 == 'F') && (c2 == 'd' || c2 == 'D') && (c3 == 'p' || c3 == 'P') ->
                  reverse rest <> ".dietpdf.pdf"
                _anyOtherCase ->
                  inputPDF <> ".dietpdf.pdf"

    outputFileExists <- liftIO (doesFileExist outputPDF)

    when (outputFileExists && overwriteFile' == DoNotOverwriteFile) $
      throwE CannotOverwriteFile

    -- Read the optimized PDF and optimize it further.
    tryF (readPDF pdfToOptimize) >>= \case
      Right document -> optimize outputPDF document settings
      Left  anError@(ParseError (_, offset, _)) -> do
        lift $ BS.readFile inputPDF >>= hexDump (fromIntegral offset)
        throwE anError
      Left anError -> throwE anError

runApp (HashOptions inputPDF) = readPDF inputPDF >>= objectHashes

runApp (EncodeOptions codec inputFile) =
  readByteString inputFile >>= encodeByteString codec

runApp (DecodeOptions codec inputFile) =
  readByteString inputFile >>= decodeByteString codec

runApp (PredictOptions predictor width components inputFile) =
  readByteString inputFile >>= predictByteString predictor width components

runApp (UnpredictOptions predictor width components inputFile) =
  readByteString inputFile >>= unpredictByteString predictor width components

runApp (HumanOptions inputFile) = readByteString inputFile >>= humanByteString

runApp (StatOptions inputPDF) = do
  let filenames = takeFileName <$> inputPDF
  pdfs <- mapM readPDF inputPDF
  pdfSizes <- mapM (lift . getFileSize) inputPDF
  showStat (zip3 filenames pdfSizes pdfs)

runApp (GetOptions objectNumber inputPDF) =
  readPDF inputPDF >>= getObjectByNumber objectNumber

{-|
Parser information for application options.
-}
options :: ParserInfo AppOptions
options = info
  (appOptions <**> helper)
  (fullDesc <> progDesc "Reduce PDF file size or analyze PDF file"
            <> header "dietpdf - reduce PDF file size"
  )

{-|
Main entry point of the application.
-}
main :: IO ()
main = runExceptT (runApp =<< lift (execParser options)) >>= \case
  Right _anyValue -> return ()
  Left anError    -> print anError
