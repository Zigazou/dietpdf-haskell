module Command.Hash
  ( objectHashes
  ) where

import           Pdf.Document.Document          ( PDFDocument
                                                , toList
                                                )
import           Util.UnifiedError              ( FallibleT )
import qualified Data.Text                     as T
import           Pdf.Object.Signature           ( streamHash )
import           Util.Logging                   ( sayF )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFIndirectObjectWithStream
                                                  )
                                                )
import           Data.Set                       ( Set
                                                , empty
                                                , insert
                                                , member
                                                )
import           Data.Foldable                  ( foldrM )
import           Data.Maybe                     ( catMaybes )

data ObjectHash = ObjectHash !Int !T.Text

instance Eq ObjectHash where
  (==) :: ObjectHash -> ObjectHash -> Bool
  (ObjectHash _ hash1) == (ObjectHash _ hash2) = hash1 == hash2

instance Ord ObjectHash where
  compare :: ObjectHash -> ObjectHash -> Ordering
  (ObjectHash _ hash1) `compare` (ObjectHash _ hash2) = hash1 `compare` hash2

objectHashes :: PDFDocument -> FallibleT IO ()
objectHashes document = do
  let
    hashes :: [ObjectHash]
    hashes = catMaybes $ objectHash <$> toList document

  _ <- foldrM printHashForObject empty hashes
  return ()
 where
  objectHash :: PDFObject -> Maybe ObjectHash
  objectHash object@(PDFIndirectObjectWithStream number _ _ _) =
    ObjectHash number . T.pack . show <$> streamHash object
  objectHash _anyOtherObject = Nothing

  printHashForObject
    :: ObjectHash -> Set ObjectHash -> FallibleT IO (Set ObjectHash)
  printHashForObject oHash@(ObjectHash number hash) hashFound =
    if member oHash hashFound
      then do
        sayF $ T.concat [hash, "\t", T.pack (show number), " duplicate!"]
        return hashFound
      else do
        sayF $ T.concat [hash, "\t", T.pack (show number)]
        return (insert oHash hashFound)
