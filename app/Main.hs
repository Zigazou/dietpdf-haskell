{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Main
  ( main
  ) where

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

dietPDFOptions :: Parser DietPDFOptions
dietPDFOptions = subparser
  (  command
      "info"
      (info
        (   InfoOptions
        <$> argument str (metavar "IN" <> help "PDF file to analyze")
        )
        (progDesc "Print information about a PDF file")
      )
  <> command
       "optimize"
       (info
         (   OptimizeOptions
         <$> switch (short 'v' <> long "verbose" <> help "Verbose output")
         <*> argument str (metavar "IN" <> help "PDF file to process")
         <*> argument str (metavar "OUT" <> help "PDF file to create")
         )
         (progDesc "Optimize a PDF file")
       )
  )

runDietPDF :: DietPDFOptions -> IO ()
runDietPDF (InfoOptions inputPDF) = do
  parsed <- pdfParse <$> BS.readFile inputPDF
  case parsed of
    Left  err     -> print err
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

main :: IO ()
main = runDietPDF =<< execParser opts
 where
  opts = info
    (dietPDFOptions <**> helper)
    (fullDesc <> progDesc "Reduce PDF file size or analyze PDF file" <> header
      "dietpdf - reduce PDF file size"
    )
