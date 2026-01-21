{-|
Determine the appropriate optimization pass for a `PDFObject`.

This module inspects object dictionaries and streams to classify optimizations
into categories:

* XML streams → `XMLOptimization`
* JPEG images (magic `\xff\xd8`) → `JPGOptimization`
* Graphics content streams → `GfxOptimization`
* TrueType font streams → `TTFOptimization`
* Object streams → `ObjectStreamOptimization`
* Cross-reference streams → `XRefStreamOptimization`
* Otherwise → `NoOptimization`

Detection uses keys like "Subtype" and "Type" and, when needed, parsers for
graphics (`gfxParse`) and TrueType fonts (`ttfParse`).
-}
module PDF.Processing.WhatOptimizationFor
  ( whatOptimizationFor
  )
where
import Data.ByteString (ByteString)
import Data.Either (isRight)
import Data.Logging (Logging)
import Data.PDF.OptimizationType
  ( OptimizationType (GfxOptimization, ICCOptimization, JP2Optimization, JPGOptimization, NoOptimization, ObjectStreamOptimization, RawBitmapOptimization, TTFOptimization, XMLOptimization, XRefStreamOptimization)
  )
import Data.PDF.PDFObject (PDFObject (PDFName, PDFNumber))
import Data.PDF.PDFWork (PDFWork, tryP)
import Data.Sequence qualified as SQ

import Font.TrueType.Parser.Font (ttfParse)

import PDF.Graphics.Parser.Stream (gfxParse)
import PDF.Object.State (getStream, getValue)

import Util.ByteString (cut)

{-
Identify the image file type based on magic numbers.

It can identify JPEG, JPEG 2000, ICC profiles, GFX streams.

It defaults to RawBitmapOptimization if no known type is found.
-}
identifyStreamContent
  :: Logging m
  => PDFObject
  -> PDFWork m (Maybe OptimizationType)
identifyStreamContent object = do
  objectType       <- getValue "Type" object
  objectSubType    <- getValue "Subtype" object
  bitsPerComponent <- getValue "BitsPerComponent" object >>= \case
    Just (PDFNumber n) -> return (Just (n == 8))
    _other             -> return (Just True)

  tryP (getStream object) >>= \case
    Right stream -> return $ identifyWithMagic objectType
                                               objectSubType
                                               bitsPerComponent
                                               stream
    _noStream    -> return Nothing

 where
  identifyWithMagic
    :: Maybe PDFObject
    -> Maybe PDFObject
    -> Maybe Bool
    -> ByteString
    -> Maybe OptimizationType
  identifyWithMagic objectType objectSubType bitsPerComponent stream
    | objectSubType == Just (PDFName "XML")               = Nothing
    | objectType == Just (PDFName "ObjStm")               = Nothing
    | objectType == Just (PDFName "XRef")                 = Nothing
    | objectType == Just (PDFName "Pattern")              = Nothing
    | bitsPerComponent == Just False                      = Nothing
    | cut 36 4 stream == "acsp"                           = Just ICCOptimization
    | cut 0 2 stream == "\xff\xd8"                        = Just JPGOptimization
    | cut 0 12 stream == "\x00\x00\x00\x0cjP  \r\n\x87\n" = Just JP2Optimization
    | objectSubType == Just (PDFName "Image")             = Nothing
    | isRight (ttfParse stream)                           = Just TTFOptimization
    | otherwise = case gfxParse stream of
        Right SQ.Empty -> Nothing
        Right _        -> Just GfxOptimization
        _notGfx        -> Nothing

{-|
Classify a `PDFObject` into an `OptimizationType`.

Heuristics:

* `Subtype = XML` → XML optimization.
* `Subtype = Image` and stream begins with JPEG magic → JPG optimization.
* `Subtype = Form` → analyze stream with `gfxParse`; non-empty graphics →
  graphics optimization.
* `Type = ObjStm` → object stream optimization.
* `Type = XRef` → xref stream optimization.
* `Type = Pattern` → graphics optimization.
* If none of the above, attempt `ttfParse` on the stream; success → TTF
  optimization. Otherwise, analyze with `gfxParse` to decide graphics or no
  optimization.
-}
whatOptimizationFor :: Logging m => PDFObject -> PDFWork m OptimizationType
whatOptimizationFor object =
  identifyStreamContent object >>= \case
    Just optType -> return optType
    Nothing -> getValue "Subtype" object >>= \case
      Just (PDFName "XML") -> return XMLOptimization
      Just (PDFName "Image") -> return RawBitmapOptimization
      Just (PDFName "Form") -> return NoOptimization
      _notXMLorImage -> getValue "Type" object >>= \case
        Just (PDFName "ObjStm")     -> return ObjectStreamOptimization
        Just (PDFName "XRef")       -> return XRefStreamOptimization
        Just (PDFName "Pattern")    -> return GfxOptimization
        Just (PDFName _unknownType) -> return NoOptimization
        _anyOtherCase -> return NoOptimization
