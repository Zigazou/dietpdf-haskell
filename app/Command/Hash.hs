module Command.Hash
  ( objectHashes
  ) where

import Data.Context (ctx)
import Data.Fallible (FallibleT)
import Data.Foldable (foldrM)
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.PDF.PDFDocument (PDFDocument, toList)
import Data.PDF.PDFObject (PDFObject (PDFIndirectObjectWithStream))
import Data.PDF.PDFWork (PDFWork, evalPDFWork, sayP, withContext)
import Data.Set (Set, empty, insert, member)
import Data.Text qualified as T

import PDF.Object.Signature (streamHash)

type ObjectHash :: Type
data ObjectHash = ObjectHash !Int !T.Text

instance Eq ObjectHash where
  (==) :: ObjectHash -> ObjectHash -> Bool
  (ObjectHash _ hash1) == (ObjectHash _ hash2) = hash1 == hash2

instance Ord ObjectHash where
  compare :: ObjectHash -> ObjectHash -> Ordering
  (ObjectHash _ hash1) `compare` (ObjectHash _ hash2) = hash1 `compare` hash2

objectHashes :: PDFDocument -> FallibleT IO ()
objectHashes document = evalPDFWork $ do
  let
    hashes :: [ObjectHash]
    hashes = mapMaybe objectHash (toList document)

  _ <- foldrM printHashForObject empty hashes
  return ()
 where
  objectHash :: PDFObject -> Maybe ObjectHash
  objectHash object@(PDFIndirectObjectWithStream number _ _ _) =
    ObjectHash number . T.pack . show <$> streamHash object
  objectHash _anyOtherObject = Nothing

  printHashForObject
    :: ObjectHash -> Set ObjectHash -> PDFWork IO (Set ObjectHash)
  printHashForObject oHash@(ObjectHash number hash) hashFound =
    withContext (ctx ("objecthashes" :: String)) $ do
      if member oHash hashFound
        then do
          sayP $ T.concat [hash, "\t", T.pack (show number), " duplicate!"]
          return hashFound
        else do
          sayP $ T.concat [hash, "\t", T.pack (show number)]
          return (insert oHash hashFound)
