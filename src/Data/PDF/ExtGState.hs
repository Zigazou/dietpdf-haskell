{-|
Extended graphics state (ExtGState) construction.

In PDF, an /ExtGState dictionary collects graphics parameters (line width,
dash pattern, rendering intent, font selection, etc.). This module provides a
minimal builder that derives such a dictionary from a sequence of graphics
`Command`s.

Only a subset of graphics operators is currently translated; unsupported
commands are ignored.
-}
module Data.PDF.ExtGState
  ( ExtGState
  , mkExtGState
  )
where

import Data.Array (Array)
import Data.Kind (Type)
import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
  ( GFXObject (GFXArray, GFXBool, GFXComment, GFXDictionary, GFXHexString, GFXName, GFXNull, GFXNumber, GFXReference, GFXString)
  , GSOperator (GSSetColourRenderingIntent, GSSetFlatnessTolerance, GSSetLineCap, GSSetLineDashPattern, GSSetLineJoin, GSSetLineWidth, GSSetMiterLimit, GSSetTextFont)
  )
import Data.PDF.PDFObject
  ( PDFObject (PDFArray, PDFBool, PDFComment, PDFDictionary, PDFHexString, PDFName, PDFNull, PDFNumber, PDFReference, PDFString)
  )
import Data.Sequence (Seq (Empty, (:<|)))

import Util.Dictionary (Dictionary, mkDictionary)

{-|
An /ExtGState dictionary represented as a `Dictionary` of `PDFObject`s.

The keys follow the PDF specification conventions (for example, @"LW"@ for line
width, @"LC"@ for line cap style, @"D"@ for dash pattern, ...).
-}
type ExtGState :: Type
type ExtGState = Dictionary PDFObject

{-|
Converts a graphics-level object (`GFXObject`) into a serializable `PDFObject`.

Unsupported values are mapped to `PDFNull`.
-}
gfxToObject :: GFXObject -> PDFObject
gfxToObject (GFXNumber value)          = PDFNumber value
gfxToObject (GFXName name)             = PDFName name
gfxToObject (GFXString string)         = PDFString string
gfxToObject (GFXHexString string)      = PDFHexString string
gfxToObject (GFXBool value)            = PDFBool value
gfxToObject GFXNull                    = PDFNull
gfxToObject (GFXComment string)        = PDFComment string
gfxToObject (GFXReference major minor) = PDFReference major minor
gfxToObject (GFXArray array)           = PDFArray . fmap gfxToObject $ array
gfxToObject (GFXDictionary dict)       = PDFDictionary . fmap gfxToObject $ dict
gfxToObject _anyOtherObject            = PDFNull

{-|
Translates a single graphics `Command` into an /ExtGState fragment.

If the operator is not handled, returns `mempty`.
-}
newState :: Command -> ExtGState
newState (Command GSSetColourRenderingIntent (intent :<| Empty)) =
  mkDictionary [("RI", gfxToObject intent)]

newState (Command GSSetLineWidth (lineWidth :<| Empty)) =
  mkDictionary [("LW", gfxToObject lineWidth)]

newState (Command GSSetLineCap (lineCap :<| Empty)) =
  mkDictionary [("LC", gfxToObject lineCap)]

newState (Command GSSetLineJoin (lineJoin :<| Empty)) =
  mkDictionary [("LJ", gfxToObject lineJoin)]

newState (Command GSSetMiterLimit (miterLimit :<| Empty)) =
  mkDictionary [("ML", gfxToObject miterLimit)]

newState (Command GSSetLineDashPattern (array :<| phase :<| Empty)) =
  mkDictionary [("D", PDFArray (PDFArray (gfxToObject array :<| Empty) :<| gfxToObject phase :<| Empty))]

newState (Command GSSetTextFont (name :<| size :<| Empty)) =
  mkDictionary [("Font", PDFArray (gfxToObject name :<| gfxToObject size :<| Empty))]

newState (Command GSSetFlatnessTolerance (tolerance :<| Empty)) =
  mkDictionary [("FL", gfxToObject tolerance)]

newState _anyOtherCommand = mempty

{-|
Builds an /ExtGState dictionary from an array of graphics commands.

Each command contributes a small dictionary, and all contributions are merged
left-to-right.
-}
mkExtGState :: Array Command -> ExtGState
mkExtGState Empty              = mempty
mkExtGState (command :<| rest) = newState command <> mkExtGState rest
