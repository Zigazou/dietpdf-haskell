module PDF.Document.OptimizeOptionalDictionaryEntries
  ( optimizeOptionalDictionaryEntries
  ) where

import Data.ByteString qualified as BS
import Data.PDF.PDFObject (PDFObject (PDFBool, PDFName, PDFNumber), mkPDFArray)
import Data.PDF.PDFWork (PDFWork, evalPDFWorkT)

import PDF.Object.Object.Properties (getValueForKey, setValueForKey)
import PDF.Processing.PDFWork (deepMapP)

isOptionalType :: PDFObject -> Bool
isOptionalType (PDFName "Annot")              = True
isOptionalType (PDFName "ExtGState")          = True
isOptionalType (PDFName "EmbeddedFile")       = True
isOptionalType (PDFName "CollectionItem")     = True
isOptionalType (PDFName "CollectionSubitem")  = True
isOptionalType (PDFName "Pattern")            = True
isOptionalType (PDFName "XObject")            = True
isOptionalType (PDFName "Group")              = True
isOptionalType (PDFName "Encoding")           = True
isOptionalType (PDFName "Halftone")           = True
isOptionalType (PDFName "Mask")               = True
isOptionalType (PDFName "Outlines")           = True
isOptionalType (PDFName "Collection")         = True
isOptionalType (PDFName "CollectionSchema")   = True
isOptionalType (PDFName "CollectionField")    = True
isOptionalType (PDFName "CollectionSort")     = True
isOptionalType (PDFName "PageLabel")          = True
isOptionalType (PDFName "Thread")             = True
isOptionalType (PDFName "Bead")               = True
isOptionalType (PDFName "Trans")              = True
isOptionalType (PDFName "NavNode")            = True
isOptionalType (PDFName "EncryptedPayload")   = True
isOptionalType (PDFName "Border")             = True
isOptionalType (PDFName "Action")             = True
isOptionalType (PDFName "SigFieldLock")       = True
isOptionalType (PDFName "SV")                 = True
isOptionalType (PDFName "SVCert")             = True
isOptionalType (PDFName "P")                  = True
isOptionalType (PDFName "SigRef")             = True
isOptionalType (PDFName "TransformParams")    = True
isOptionalType (PDFName "Viewport")           = True
isOptionalType (PDFName "Measure")            = True
isOptionalType (PDFName "NumberFormat")       = True
isOptionalType (PDFName "Requirement")        = True
isOptionalType (PDFName "ReqHandler")         = True
isOptionalType (PDFName "Rendition")          = True
isOptionalType (PDFName "MediaCriteria")      = True
isOptionalType (PDFName "MinBitDepth")        = True
isOptionalType (PDFName "MinScreenSize")      = True
isOptionalType (PDFName "MediaClip")          = True
isOptionalType (PDFName "MediaPermissions")   = True
isOptionalType (PDFName "MediaPlayParams")    = True
isOptionalType (PDFName "MediaDuration")      = True
isOptionalType (PDFName "MediaScreenParams")  = True
isOptionalType (PDFName "FWParams")           = True
isOptionalType (PDFName "MediaOffset")        = True
isOptionalType (PDFName "Timespan")           = True
isOptionalType (PDFName "MediaPlayers")       = True
isOptionalType (PDFName "MediaPlayerInfo")    = True
isOptionalType (PDFName "SoftwareIdentifier") = True
isOptionalType (PDFName "Sound")              = True
isOptionalType (PDFName "3D")                 = True
isOptionalType (PDFName "3DRef")              = True
isOptionalType (PDFName "3DView")             = True
isOptionalType (PDFName "3DBG")               = True
isOptionalType (PDFName "3DRenderMode")       = True
isOptionalType (PDFName "3DLightingScheme")   = True
isOptionalType (PDFName "3DCrossSection")     = True
isOptionalType (PDFName "3DNode")             = True
isOptionalType (PDFName "StructElem")         = True
isOptionalType (PDFName "SpiderContentSet")   = True
isOptionalType (PDFName "OutputIntent")       = True
isOptionalType (PDFName "OPI")                = True
isOptionalType _anyOtherType                  = False

defaultValues :: [(BS.ByteString, PDFObject)]
defaultValues =
  [ ("AccurateScreens", PDFBool False)
  , ("AddRevInfo", PDFBool False)
  , ("AntiAlias", PDFBool False)
  , ("AuthEvent", PDFName "DocOpen")
  , ("AvgWidth", PDFNumber 0)
  , ("BaselineShift", PDFNumber 0)
  , ("BaseState", PDFName "ON")
  , ("BlackIs1", PDFBool False)
  , ("BlackPoint", mkPDFArray [PDFNumber 0, PDFNumber 0, PDFNumber 0])
  , ("BlockAlign", PDFName "Before")
  , ("Border", mkPDFArray [PDFNumber 0, PDFNumber 0, PDFNumber 1])
  , ("BorderStyle", PDFName "None")
  , ("BorderThickness", PDFNumber 0)
  , ("Cap", PDFBool False)
  , ("CenterWindow", PDFBool False)
  , ("CFM", PDFName "None")
  , ("checked", PDFName "off")
  , ("Checked", PDFName "off")
  , ("Colors", PDFNumber 1)
  , ("ColorType", PDFName "Spot")
  , ("ColSpan", PDFNumber 1)
  , ("ColumnCount", PDFNumber 1)
  , ("ContinueList", PDFBool False)
  , ("DamagedRowsBeforeError", PDFNumber 0)
  , ("DefaultForPrinting", PDFBool False)
  , ("Direction", PDFName "L2R")
  , ("DIS", PDFName "U")
  , ("DisplayDocTitle", PDFBool False)
  , ("EarlyChange", PDFNumber 1)
  , ("EncodedByteAlign", PDFBool False)
  , ("Encoding", PDFName "PDFDocEncoding")
  , ("EncryptMetadata", PDFBool True)
  , ("EndIndent", PDFNumber 0)
  , ("EndOfBlock", PDFBool True)
  , ("EndOfLine", PDFBool False)
  , ("Extend", mkPDFArray [PDFBool False, PDFBool False])
  , ("FitWindow", PDFBool False)
  , ("Flags", PDFNumber 0)
  , ("FormType", PDFNumber 1)
  , ("FWPosition", mkPDFArray [PDFNumber 0.5, PDFNumber 0.5])
  , ("GlyphOrientationVertical", PDFName "Auto")
  , ("HAlign", PDFName "Far")
  , ("Height", PDFName "Auto")
  , ("HideMenubar", PDFBool False)
  , ("HideToolbar", PDFBool False)
  , ("HOffset", PDFNumber 18)
  , ("ImageMask", PDFBool False)
  , ("InlineAlign", PDFName "Start")
  , ("Interpolate", PDFBool False)
  , ("IsMap", PDFBool False)
  , ("Leading", PDFNumber 0)
  , ("Length", PDFNumber 40)
  , ("LineHeight", PDFName "Normal")
  , ("ListMode", PDFName "AllPages")
  , ("ListNumbering", PDFName "None")
  , ("LockDocument", PDFName "auto")
  , ("Locked", mkPDFArray [])
  , ("Marked", PDFBool False)
  , ("Matrix", mkPDFArray [ PDFNumber 1
                          , PDFNumber 0
                          , PDFNumber 0
                          , PDFNumber 1
                          , PDFNumber 0
                          , PDFNumber 0
                          ]
    )
  , ("MaxWidth", PDFNumber 0)
  , ("MissingWidth", PDFNumber 0)
  , ("Mix", PDFBool False)
  , ("Mode", PDFName "Once")
  , ("NavigationPane", PDFBool False)
  , ("NeedAppearances", PDFBool False)
  , ("NeedsRendering", PDFBool False)
  , ("NonFullScreenPageMode", PDFName "UseNone")
  , ("OCGs", mkPDFArray [])
  , ("Open", PDFBool False)
  , ("Operation", PDFName "Play")
  , ("Order", PDFNumber 1)
  , ("Overprint", PDFBool False)
  , ("Padding", PDFNumber 0)
  , ("PageLayout", PDFName "SinglePage")
  , ("PageMode", PDFName "UseNone")
  , ("Params", mkPDFArray [])
  , ("PassContextClick", PDFBool False)
  , ("PhoneticAlphabet", PDFName "ipa")
  , ("PlayCount", PDFNumber (-1))
  , ("Poster", PDFBool False)
  , ("Predictor", PDFNumber 1)
  , ("PreserveRB", PDFBool True)
  , ("PrintScaling", PDFName "AppDefault")
  , ("Rate", PDFNumber 1)
  , ("Rename", PDFBool True)
  , ("Repeat", PDFBool False)
  , ("Rotate", PDFNumber 0)
  , ("RowSpan", PDFNumber 1)
  , ("Rows", PDFNumber 0)
  , ("RubyAlign", PDFName "Distribute")
  , ("RubyPosition", PDFName "Before")
  , ("ShowControls", PDFBool False)
  , ("SigFlags", PDFNumber 0)
  , ("SMaskInData", PDFNumber 0)
  , ("SpaceAfter", PDFNumber 0)
  , ("SpaceBefore", PDFNumber 0)
  , ("Speed", PDFNumber 1)
  , ("StartIndent", PDFNumber 0)
  , ("StemH", PDFNumber 0)
  , ("StmF", PDFName "Identity")
  , ("StrF", PDFName "Identity")
  , ("Style", PDFName "Embedded")
  , ("Suspects", PDFBool False)
  , ("Syncrhonous", PDFBool False)
  , ("TBorderStyle", PDFName "None")
  , ("TextAlign", PDFName "Start")
  , ("TextDecorationType", PDFName "None")
  , ("TextIndent", PDFNumber 0)
  , ("TextPosition", PDFName "Normal")
  , ("Tint", PDFNumber 1)
  , ("TPadding", PDFNumber 0)
  , ("Transparency", PDFBool True)
  , ("Transparent", PDFBool False)
  , ("Trapped", PDFName "Unknown")
  , ("UserProperties", PDFBool False)
  , ("UserUnit", PDFNumber 1)
  , ("VAlign", PDFName "Near")
  , ("View", PDFName "D")
  , ("VOffset", PDFNumber 18)
  , ("Volume", PDFNumber 1)
  , ("WMode", PDFNumber 0)
  , ("WritingMode", PDFName "LrTb")
  , ("XHeight", PDFNumber 0)
  , ("Zoom", PDFNumber 0)
  ]

removeEntryIfDefaultP
  :: Monad m
  => (BS.ByteString, PDFObject)
  -> PDFObject
  -> PDFWork m PDFObject
removeEntryIfDefaultP (key, defaultValue) object =
  return $ case getValueForKey key object of
    Just value | value == defaultValue -> setValueForKey key Nothing object
    _anyOtherValue                     -> object

removeEntryIfDefaults :: [(BS.ByteString, PDFObject)] -> PDFObject -> PDFObject
removeEntryIfDefaults defaults object = foldl deepGo object defaults
 where
  deepGo :: PDFObject -> (BS.ByteString, PDFObject) -> PDFObject
  deepGo object' keyValue =
    case evalPDFWorkT (deepMapP (removeEntryIfDefaultP keyValue) object') of
      Right (Right object'') -> object''
      _anyOtherValue         -> object'

removeOptionalTypeEntries :: PDFObject -> PDFObject
removeOptionalTypeEntries object =
  case getValueForKey "Type" object of
    Just aType | isOptionalType aType -> setValueForKey "Type" Nothing object
    _anythingElse                     -> object

optimizeOptionalDictionaryEntries :: PDFObject -> PDFObject
optimizeOptionalDictionaryEntries  = removeEntryIfDefaults defaultValues
                                   . removeOptionalTypeEntries
