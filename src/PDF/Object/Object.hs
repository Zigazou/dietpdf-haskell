{-|
This module defines what is a PDF object and functions in relation with the
PDF specification.
-}
module PDF.Object.Object
  ( -- * PDF object
    PDFObject
    ( PDFComment
    , PDFVersion
    , PDFEndOfFile
    , PDFNumber
    , PDFKeyword
    , PDFName
    , PDFString
    , PDFHexString
    , PDFReference
    , PDFArray
    , PDFDictionary
    , PDFIndirectObject
    , PDFIndirectObjectWithStream
    , PDFIndirectObjectWithGraphics
    , PDFObjectStream
    , PDFXRefStream
    , PDFBool
    , PDFNull
    , PDFXRef
    , PDFTrailer
    , PDFStartXRef
    )
  , mkEmptyPDFArray
  , mkEmptyPDFDictionary
  , mkPDFArray
  , mkPDFDictionary

    -- * Conversion
  , fromPDFObject
  , mkPDFNumber
  , ToPDFNumber

    -- * Getting info about a `PDFObject`
  , hasKey
  , hasDictionary
  , hasStream

    -- * PDF indirect object
  , updateStream
  , getObjectNumber

    -- * PDF characters
  , isDelimiter
  , isPlusMinus
  , isWhiteSpace
  , isSpace
  , isKeywordCharacter
  , isOctal
  , isStringEscapeSequence
  , isStringRegularChar
  , isNameRegularChar
  , spaceIfNeeded

    -- * XRef
  , xrefCount
  , inUseEntry
  , freeEntry
  , XRefState(InUseEntry, FreeEntry)
  , XRefEntry(XRefEntry, xreOffset, xreGeneration, xreState)
  , XRefSubsection(XRefSubsection, xrssStart, xrssCount, xrssEntries)

    -- * Partitioning
  , isIndirect
  , isHeader
  , isTrailer
  ) where

import Data.PDF.PDFObject
    ( PDFObject (PDFArray, PDFBool, PDFComment, PDFDictionary, PDFEndOfFile, PDFHexString, PDFIndirectObject, PDFIndirectObjectWithGraphics, PDFIndirectObjectWithStream, PDFKeyword, PDFName, PDFNull, PDFNumber, PDFObjectStream, PDFReference, PDFStartXRef, PDFString, PDFTrailer, PDFVersion, PDFXRef, PDFXRefStream)
    , mkEmptyPDFArray
    , mkEmptyPDFDictionary
    , mkPDFArray
    , mkPDFDictionary
    )
import Data.PDF.XRefEntry
    ( XRefEntry (XRefEntry, xreGeneration, xreOffset, xreState)
    , freeEntry
    , inUseEntry
    )
import Data.PDF.XRefState (XRefState (FreeEntry, InUseEntry))
import Data.PDF.XRefSubsection
    ( XRefSubsection (XRefSubsection, xrssCount, xrssEntries, xrssStart)
    )

import PDF.Object.Object.Delimiter (spaceIfNeeded)
import PDF.Object.Object.FromPDFObject (fromPDFObject)
import PDF.Object.Object.PDFCharacter
    ( isDelimiter
    , isKeywordCharacter
    , isNameRegularChar
    , isOctal
    , isPlusMinus
    , isSpace
    , isStringEscapeSequence
    , isStringRegularChar
    , isWhiteSpace
    )
import PDF.Object.Object.Properties
    ( getObjectNumber
    , hasDictionary
    , hasKey
    , hasStream
    , isHeader
    , isIndirect
    , isTrailer
    , updateStream
    , xrefCount
    )
import PDF.Object.Object.ToPDFNumber (ToPDFNumber, mkPDFNumber)
