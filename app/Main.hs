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
                                                )
import           Pdf.Document.Document          ( PDFDocument )
import           Pdf.Document.Parser            ( pdfParse )
import           System.IO.Error                ( isDoesNotExistError )
import           Util.Errors                    ( UnifiedError(UnableToOpenFile)
                                                )

readPDF :: FilePath -> IO (Either UnifiedError PDFDocument)
readPDF filename = do
  bytesE <- tryJust (guard . isDoesNotExistError) (BS.readFile filename)

  return $ case bytesE of
    Left  _     -> Left UnableToOpenFile
    Right bytes -> pdfParse bytes

runApp :: AppOptions -> IO ()
runApp (InfoOptions inputPDF) = readPDF inputPDF >>= \case
  Left  err     -> print err
  Right objects -> showInfo objects
runApp (ExtractOptions objectNumber inputPDF) = readPDF inputPDF >>= \case
  Left  err     -> print err
  Right objects -> extract objectNumber objects
runApp (OptimizeOptions verbose inputPDF outputPDF) =
  readPDF inputPDF >>= \case
    Left  err     -> print err
    Right objects -> optimize verbose objects outputPDF

main :: IO ()
main = runApp =<< execParser opts
 where
  opts = info
    (appOptions <**> helper)
    (fullDesc <> progDesc "Reduce PDF file size or analyze PDF file" <> header
      "dietpdf - reduce PDF file size"
    )
