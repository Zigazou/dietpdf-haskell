{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
module Main
  ( main
  ) where

import           AppOptions                     ( AppOptions
                                                  ( ExtractOptions
                                                  , InfoOptions
                                                  , OptimizeOptions
                                                  )
                                                , appOptions
                                                )
import           Control.Exception              ( tryJust )
import           Control.Monad                  ( guard )
import qualified Data.ByteString               as BS
import           Extract                        ( extract )
import           Info                           ( showInfo )
import           Optimize                       ( optimize )
import           Options.Applicative            ( (<**>)
                                                , execParser
                                                , fullDesc
                                                , header
                                                , helper
                                                , info
                                                , progDesc
                                                , ParserInfo
                                                )
import           Pdf.Document.Document          ( PDFDocument )
import           Pdf.Document.Parser            ( pdfParse )
import           System.IO.Error                ( isDoesNotExistError )
import           Util.UnifiedError              ( UnifiedError
                                                  ( UnableToOpenFile
                                                  , ParseError
                                                  )
                                                , FallibleT
                                                , tryF
                                                )
import           Control.Monad.Trans.Except     ( throwE
                                                , runExceptT
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Hexdump                        ( Cfg
                                                , startByte
                                                , defaultCfg
                                                , prettyHexCfg
                                                )

readPDF :: FilePath -> FallibleT IO PDFDocument
readPDF filename = do
  lift (tryJust (guard . isDoesNotExistError) (BS.readFile filename)) >>= \case
    Right bytes -> pdfParse bytes
    Left  _     -> throwE UnableToOpenFile

hexCfg :: Int -> Cfg
hexCfg offset = defaultCfg { startByte = offset }

hexDump :: Int -> BS.ByteString -> IO ()
hexDump offset bytes = do
  let bytes'  = BS.take 256 (BS.drop offset bytes)

  putStrLn $ prettyHexCfg (hexCfg offset) bytes'

runApp :: AppOptions -> FallibleT IO ()
runApp (InfoOptions inputPDF) = readPDF inputPDF >>= showInfo
runApp (ExtractOptions objectNumber inputPDF) =
  readPDF inputPDF >>= extract objectNumber
runApp (OptimizeOptions inputPDF outputPDF) = do
  tryF (readPDF inputPDF) >>= \case
    Right document -> optimize outputPDF document
    Left anError@(ParseError (_, offset, _)) -> do
      lift $ BS.readFile inputPDF >>= hexDump (fromIntegral offset)
      throwE anError
    Left anError -> throwE anError

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
