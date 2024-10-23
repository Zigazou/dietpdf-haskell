module Command.Info
  ( showInfo
  ) where

import Control.Monad (forM_)

import Data.Fallible (FallibleT)
import Data.PDF.ObjectInfo
    ( ObjectInfo (oCategory, oDescription, oEmbedded, oNumber, oOffset, oStream)
    , StreamInfo (sFilteredSize, sUnfilteredSize)
    )
import Data.PDF.PDFDocument (PDFDocument)
import Data.PDF.PDFWork (evalPDFWork)
import Data.Text qualified as T

import PDF.Processing.ObjectInfo (objectInfo)

import Util.Display (disp)

header :: [String]
header = [ "type"
         , "id"
         , "offset"
         , "compressed"
         , "uncompressed"
         , "container"
         , "description"
         ]

showInfo :: PDFDocument -> FallibleT IO ()
showInfo document = do
  disp header

  forM_ document $ \object -> do
    info <- evalPDFWork (objectInfo object Nothing)
    let objectNumber = oNumber info

    disp
      [ show (oCategory info)
      , maybe "" show objectNumber
      , maybe "" show (oOffset info)
      , maybe "" (show . sFilteredSize) (oStream info)
      , maybe "" (show . sUnfilteredSize) (oStream info)
      , ""
      , T.unpack (oDescription info)
      ]

    forM_ (oEmbedded info) $ \embeddedObject -> do
      disp
        [ show (oCategory embeddedObject)
        , maybe "" show (oNumber embeddedObject)
        , maybe "" show (oOffset embeddedObject)
        , ""
        , ""
        , maybe "" show objectNumber
        , T.unpack (oDescription embeddedObject)
        ]