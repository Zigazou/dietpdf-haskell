{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Optimize
  ( optimize
  ) where

import           Control.Monad                  ( when )
import qualified Data.ByteString               as BS
import qualified Data.Text.Lazy.IO             as T
import           Formatting                     ( (%)
                                                , format
                                                , int
                                                )
import           Pdf.Document.Document          ( PDFDocument )
import           Pdf.Document.Encode            ( pdfEncode )
import           Pdf.Object.Object              ( objectInfo )

optimize :: Bool -> PDFDocument -> FilePath -> IO ()
optimize verbose objects outputPDF = do
  when verbose $ do
    T.putStrLn $ format ("Found " % int % " objects") (length objects)
    mapM_ (T.putStrLn . objectInfo) objects
  case pdfEncode objects of
    Left  err -> print err
    Right pdf -> BS.writeFile outputPDF pdf
