{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
module Main
  ( main
  ) where

import           Control.Monad                  ( guard
                                                )
import qualified Data.ByteString               as BS
import           Options.Applicative            ( (<**>)
                                                , execParser
                                                , fullDesc
                                                , header
                                                , helper
                                                , info
                                                , progDesc
                                                )
import           Pdf.Object.Object              ( PDFObject )
import           Pdf.Document.Parser            ( pdfParse )
import           Control.Exception              ( tryJust )
import           Util.Errors                    ( UnifiedError(UnableToOpenFile)
                                                )
import           System.IO.Error                ( isDoesNotExistError )
import           AppOptions                     ( AppOptions
                                                  ( OptimizeOptions
                                                  , InfoOptions
                                                  , ExtractOptions
                                                  )
                                                , appOptions
                                                )
import           Optimize                       ( optimize )
import           Info                           ( showInfo )
import           Extract                        ( extract )

readPDF :: FilePath -> IO (Either UnifiedError [PDFObject])
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
