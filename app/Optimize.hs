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
import           Util.UnifiedError              ( FallibleT )
import           Control.Monad.Except           ( MonadTrans(lift) )

optimize :: Bool -> FilePath -> PDFDocument -> FallibleT IO ()
optimize verbose outputPDF objects = do
  when verbose $ do
    lift . T.putStrLn $ format ("Found " % int % " objects") (length objects)
    mapM_ (lift . T.putStrLn . objectInfo) objects

  pdfEncode objects >>= lift . BS.writeFile outputPDF
