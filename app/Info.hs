{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Info
  ( showInfo
  ) where

import qualified Data.Text.Lazy.IO             as T
import           Formatting                     ( (%)
                                                , format
                                                , int
                                                )
import           Pdf.Document.Document          ( PDFDocument )
import           Pdf.Object.Linearization       ( getLinearization )
import           Pdf.Object.Object              ( objectInfo )

showInfo :: PDFDocument -> IO ()
showInfo objects = do
  T.putStrLn $ format ("Found " % int % " objects") (length objects)
  mapM_ (T.putStrLn . objectInfo) objects
  case getLinearization objects of
    Nothing            -> T.putStrLn "PDF is not linearized"
    Just linearization -> print linearization
