module Main
  ( main
  ) where

import AppOptions
    ( AppOptions (DecodeOptions, EncodeOptions, ExtractOptions, HashOptions, InfoOptions, OptimizeOptions, PredictOptions, UnpredictOptions)
    , appOptions
    )

import Command.Decode (decodeByteString)
import Command.Encode (encodeByteString)
import Command.Extract (extract)
import Command.Hash (objectHashes)
import Command.Info (showInfo)
import Command.Optimize (optimize)
import Command.Predict (predictByteString)
import Command.Unpredict (unpredictByteString)

import Control.Exception (tryJust)
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT, throwE)

import Data.ByteString qualified as BS

import Hexdump (Cfg, defaultCfg, prettyHexCfg, startByte)

import Options.Applicative
    ( ParserInfo
    , execParser
    , fullDesc
    , header
    , helper
    , info
    , progDesc
    , (<**>)
    )

import Pdf.Document.Document (PDFDocument)
import Pdf.Document.Parser (pdfParse)

import System.IO (hClose)
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (withSystemTempFile)
import System.Posix (fileSize, getFileStatus)

import External.GSOptimize (gsOptimize)
import Util.Logging (sayComparisonF)
import Util.UnifiedError
    ( FallibleT
    , UnifiedError (ParseError, UnableToOpenFile)
    , tryF
    )

readPDF :: FilePath -> FallibleT IO PDFDocument
readPDF filename = do
  lift (tryJust (guard . isDoesNotExistError) (BS.readFile filename)) >>= \case
    Right bytes -> pdfParse bytes
    Left  _     -> throwE UnableToOpenFile

readByteString :: Maybe FilePath -> FallibleT IO BS.ByteString
readByteString (Just filename) = do
  lift (tryJust (guard . isDoesNotExistError) (BS.readFile filename)) >>= \case
    Right bytes -> return bytes
    Left  _     -> throwE UnableToOpenFile
readByteString Nothing =
  lift (tryJust (guard . isDoesNotExistError) BS.getContents) >>= \case
    Right bytes -> return bytes
    Left  _     -> throwE UnableToOpenFile

getFileSize :: String -> IO Int
getFileSize path = do
    stat <- getFileStatus path
    return $ fromIntegral (fileSize stat)

hexCfg :: Int -> Cfg
hexCfg offset = defaultCfg { startByte = offset }

hexDump :: Int -> BS.ByteString -> IO ()
hexDump offset bytes = do
  let bytes' = BS.take 256 (BS.drop offset bytes)

  putStrLn $ prettyHexCfg (hexCfg offset) bytes'

runApp :: AppOptions -> FallibleT IO ()
runApp (InfoOptions inputPDF) = readPDF inputPDF >>= showInfo

runApp (ExtractOptions objectNumber inputPDF) =
  readPDF inputPDF >>= extract objectNumber

runApp (OptimizeOptions inputPDF outputPDF useGS) = do
  if useGS
    then withSystemTempFile (inputPDF <> ".ghostscript") $ \ghostscriptPDF ghostscriptHandle -> do
      -- Close the handles so external programs can use the files.
      lift $ hClose ghostscriptHandle

      -- Optimize PDF with GhostScript.
      gsOptimize inputPDF ghostscriptPDF

      -- Compare the sizes of the original and GhostScript PDFs.
      originalSize <- lift $ getFileSize inputPDF
      ghostScriptSize <- lift $ getFileSize ghostscriptPDF

      sayComparisonF "GhostScripted PDF" originalSize ghostScriptSize

      if ghostScriptSize < originalSize then go ghostscriptPDF
                                        else go inputPDF
    else
      go inputPDF
 where
  go :: FilePath -> FallibleT IO ()
  go pdfToOptimize = do
    -- Read the optimized PDF and optimize it further.
    tryF (readPDF pdfToOptimize) >>= \case
      Right document                            -> optimize outputPDF document
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

options :: ParserInfo AppOptions
options = info
  (appOptions <**> helper)
  (fullDesc <> progDesc "Reduce PDF file size or analyze PDF file" <> header
    "dietpdf - reduce PDF file size"
  )

main :: IO ()
main = runExceptT (runApp =<< lift (execParser options)) >>= \case
  Right _       -> return ()
  Left  anError -> print anError
