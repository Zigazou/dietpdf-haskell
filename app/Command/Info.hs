{-|
Displays a table of per-object information for a PDF document.

This module exposes 'showInfo', which prints a header row followed by one line
per object in the 'PDFDocument'. It uses 'objectInfo' to collect details such
as category, object number, byte offset, stream sizes (filtered/unfiltered),
and a textual description. Embedded objects are shown on subsequent lines with
their container object number populated in the "container" column.
-}
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

{-|
Column headers for the output table produced by 'showInfo'.

The columns are, in order: object type category, object id, byte offset,
compressed size, uncompressed size, container id (for embedded objects), and
human-readable description.
-}
header :: [String]
header = [ "type"
         , "id"
         , "offset"
         , "compressed"
         , "uncompressed"
         , "container"
         , "description"
         ]

{-|
Print per-object information for a 'PDFDocument' to standard output.

Behavior:

* Prints a header row using 'disp' and the predefined 'header'.
* Iterates through objects in the document. For each object, evaluates
  'objectInfo' via 'evalPDFWork' to collect details, then prints a row with
  the object's properties.
* For each embedded object listed in 'oEmbedded', prints an additional row
  with the container id populated and stream size columns left blank.

Side effects: writes rows to stdout via 'disp'.
-}
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