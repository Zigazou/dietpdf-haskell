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

type ExtGState :: Type
type ExtGState = Dictionary PDFObject

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

mkExtGState :: Array Command -> ExtGState
mkExtGState Empty              = mempty
mkExtGState (command :<| rest) = newState command <> mkExtGState rest
