{-|
Conversions and merges for PDF resource dictionaries.

PDF pages and form XObjects can contain a resource dictionary, which maps
resource categories (such as @Font@ or @XObject@) to dictionaries of concrete
resources.

This module provides helpers to:

* Convert between 'PDFObject' and dictionary representations.
* Interpret nested dictionaries as resource dictionaries.
* Merge two resource dictionaries in a category-aware way.

The keys used for categories are the PDF standard names (e.g. @"Font"@,
@"XObject"@).
-}
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

{-|
Dictionary representing a single resource-category dictionary.

This is the inner dictionary at keys like @/Font@ or @/XObject@.
-}
type ResourceDictionary :: Type
type ResourceDictionary = Dictionary PDFObject

{-|
Extract a 'ResourceDictionary' from a 'PDFObject'.

If the object is not a dictionary, this returns the empty dictionary.
-}
objectToResourceDictionary :: PDFObject -> ResourceDictionary
objectToResourceDictionary (PDFDictionary dictionary) = dictionary
objectToResourceDictionary _anyOtherObject = mempty

{-|
Extract a dictionary of resource dictionaries from a 'PDFObject'.

This expects a top-level resource dictionary mapping category names (like
@"Font"@) to dictionaries.

If the object is not a dictionary, this returns the empty dictionary.
-}
objectToResourceDictionaries :: PDFObject -> Dictionary ResourceDictionary
objectToResourceDictionaries (PDFDictionary dictionary) =
  Map.map objectToResourceDictionary dictionary
objectToResourceDictionaries _anyOtherObject = mempty

{-|
Convert a 'ResourceDictionary' back to a 'PDFObject' dictionary.
-}
resourceDictionaryToPDFObject :: ResourceDictionary -> PDFObject
resourceDictionaryToPDFObject = PDFDictionary

{-|
Convert a dictionary of resource dictionaries back to a 'PDFObject'.

Each inner dictionary is converted using 'resourceDictionaryToPDFObject'.
-}
resourceDictionariesToPDFObject :: Dictionary ResourceDictionary -> PDFObject
resourceDictionariesToPDFObject = PDFDictionary
                                . fmap resourceDictionaryToPDFObject

{-|
Interpret each value in a dictionary as a 'ResourceDictionary'.

This is a convenience wrapper around 'objectToResourceDictionary'.
-}
dictionaryToResourceDictionaries
  :: Dictionary PDFObject
  -> Dictionary ResourceDictionary
dictionaryToResourceDictionaries = Map.map objectToResourceDictionary

{-|
Merge two resource dictionaries.

The merge is performed per standard category key (e.g. @"Font"@, @"XObject"@).
For each category, the inner dictionaries are combined using the monoidal append
operation.

Empty category entries are removed from the resulting dictionary.
-}
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
