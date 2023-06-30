{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
<<<<<<< HEAD
=======
{-# LANGUAGE LambdaCase #-}
>>>>>>> 2cda7c92fcf859f588ef19db6e288dd3ad74727d
module Main
  ( main
  ) where

<<<<<<< HEAD
import           Control.Monad                  ( when )
import qualified Data.ByteString               as BS
import qualified Data.Text.Lazy.IO             as T
import           Formatting                     ( (%)
                                                , format
                                                , int
                                                )
import           Options.Applicative            ( (<**>)
                                                , Parser
                                                , argument
                                                , command
                                                , execParser
                                                , fullDesc
                                                , header
                                                , help
                                                , helper
                                                , info
                                                , long
                                                , metavar
                                                , progDesc
                                                , short
                                                , str
                                                , subparser
                                                , switch
                                                )
import           Pdf.Document.Encode            ( pdfEncode )
import           Pdf.Object.Linearization       ( getLinearization )
import           Pdf.Object.Object              ( objectInfo )
import           Pdf.Parser.Parser              ( pdfParse )
import           Util.Step                      ( runExceptT )

data DietPDFOptions
  = OptimizeOptions Bool FilePath FilePath
  | InfoOptions FilePath
=======
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
>>>>>>> 2cda7c92fcf859f588ef19db6e288dd3ad74727d

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
<<<<<<< HEAD
    Right objects -> do
      T.putStrLn $ format ("Found " % int % " objects") (length objects)
      mapM_ (T.putStrLn . objectInfo) objects
      case getLinearization objects of
        Nothing            -> T.putStrLn "PDF is not linearized"
        Just linearization -> print linearization

runDietPDF (OptimizeOptions verbose inputPDF outputPDF) = do
  parsed <- pdfParse <$> BS.readFile inputPDF
  case parsed of
    Left err -> do
      when verbose $ do
        T.putStrLn "Unable to parse PDF file"
      print err
    Right objects -> do
      when verbose $ do
        T.putStrLn $ format ("Found " % int % " objects") (length objects)
        mapM_ (T.putStrLn . objectInfo) objects

      encodingResults <- runExceptT $ pdfEncode objects
      case encodingResults of
        Left  err -> print err
        Right pdf -> BS.writeFile outputPDF pdf
=======
    Right objects -> optimize verbose objects outputPDF
>>>>>>> 2cda7c92fcf859f588ef19db6e288dd3ad74727d

main :: IO ()
main = runApp =<< execParser opts
 where
  opts = info
    (appOptions <**> helper)
    (fullDesc <> progDesc "Reduce PDF file size or analyze PDF file" <> header
      "dietpdf - reduce PDF file size"
    )
