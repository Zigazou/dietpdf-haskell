module Data.PDF.ResourceDictionary
  ( ResourceDictionary
  , objectToResourceDictionary
  , mergeResourceDictionaries
  , resourceDictionaryToPDFObject
  , dictionaryToResourceDictionaries
  , resourceDictionariesToPDFObject
  , objectToResourceDictionaries
  )
where

import Data.Kind (Type)
import Data.Map qualified as Map
import Data.PDF.PDFObject (PDFObject (PDFDictionary))

import Util.Dictionary (Dictionary)

type ResourceDictionary :: Type
type ResourceDictionary = Dictionary PDFObject

objectToResourceDictionary :: PDFObject -> ResourceDictionary
objectToResourceDictionary (PDFDictionary dictionary) = dictionary
objectToResourceDictionary _anyOtherObject = mempty

objectToResourceDictionaries :: PDFObject -> Dictionary ResourceDictionary
objectToResourceDictionaries (PDFDictionary dictionary) =
  Map.map objectToResourceDictionary dictionary
objectToResourceDictionaries _anyOtherObject = mempty

resourceDictionaryToPDFObject :: ResourceDictionary -> PDFObject
resourceDictionaryToPDFObject = PDFDictionary

resourceDictionariesToPDFObject :: Dictionary ResourceDictionary -> PDFObject
resourceDictionariesToPDFObject = PDFDictionary
                                . fmap resourceDictionaryToPDFObject

dictionaryToResourceDictionaries
  :: Dictionary PDFObject
  -> Dictionary ResourceDictionary
dictionaryToResourceDictionaries = Map.map objectToResourceDictionary

mergeResourceDictionaries
  :: Dictionary ResourceDictionary
  -> Dictionary ResourceDictionary
  -> Dictionary ResourceDictionary
mergeResourceDictionaries resDict1 resDict2 =
  let
    colorSpace1 = Map.findWithDefault mempty "ColorSpace" resDict1
    font1       = Map.findWithDefault mempty "Font"       resDict1
    xObject1    = Map.findWithDefault mempty "XObject"    resDict1
    extGState1  = Map.findWithDefault mempty "ExtGState"  resDict1
    properties1 = Map.findWithDefault mempty "Properties" resDict1
    pattern1    = Map.findWithDefault mempty "Pattern"    resDict1
    shading1    = Map.findWithDefault mempty "Shading"    resDict1
    procSet1    = Map.findWithDefault mempty "ProcSet"    resDict1

    colorSpace2 = Map.findWithDefault mempty "ColorSpace" resDict2
    font2       = Map.findWithDefault mempty "Font"       resDict2
    xObject2    = Map.findWithDefault mempty "XObject"    resDict2
    extGState2  = Map.findWithDefault mempty "ExtGState"  resDict2
    properties2 = Map.findWithDefault mempty "Properties" resDict2
    pattern2    = Map.findWithDefault mempty "Pattern"    resDict2
    shading2    = Map.findWithDefault mempty "Shading"    resDict2
    procSet2    = Map.findWithDefault mempty "ProcSet"    resDict2

  in
    Map.fromList . filter (not . null . snd) $
      [ ( "ColorSpace", colorSpace1 <> colorSpace2 )
      , ( "Font",       font1       <> font2       )
      , ( "XObject",    xObject1    <> xObject2    )
      , ( "ExtGState",  extGState1  <> extGState2  )
      , ( "Properties", properties1 <> properties2 )
      , ( "Pattern",    pattern1    <> pattern2    )
      , ( "Shading",    shading1    <> shading2    )
      , ( "ProcSet",    procSet1    <> procSet2    )
      ]
