module PDF.Document.GetAllMasks
  ( getAllMasks
  )
where
import Data.PDF.PDFDocument (PDFDocument, deepFind, toList)
import Data.PDF.PDFObject (PDFObject (PDFReference))
import Data.Set (Set)
import Data.Set qualified as Set

import PDF.Object.Object.Properties (getValueForKey)

isMaskReference :: PDFObject -> Bool
isMaskReference object = case getValueForKey "SMask" object of
  Just _        -> True
  _anyOtherCase -> False

getNumber :: PDFObject -> Int
getNumber object = case getValueForKey "SMask" object of
  Just (PDFReference major _minor) -> major
  _anyOtherCase                    -> 0

getAllMasks :: PDFDocument -> Set Int
getAllMasks = Set.delete 0
            . Set.fromList
            . fmap getNumber
            . toList
            . deepFind isMaskReference
