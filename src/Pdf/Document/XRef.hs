module Pdf.Document.XRef
  ( -- * XRef generation
    calcOffsets
  , encodeObject
  , xrefTable
  , xrefStreamTable
  , objectsNumberRange
  , PDFObjects
  , EncodedObject(..)
  , EncodedObjects
  , ObjectOffsets
  ) where

import           Data.Foldable                  ( foldl' )
import qualified Data.IntMap.Strict            as IM
import           Data.Ix                        ( range
                                                , rangeSize
                                                )
import           Data.Sort                      ( sort )
import           Pdf.Object.Object              ( PDFObject
                                                  ( PDFXRef
                                                  , PDFXRefStream
                                                  , PDFName
                                                  )
                                                , XRefSubsection(XRefSubsection)
                                                , freeEntry
                                                , inUseEntry
                                                , mkPDFArray
                                                , ToPDFNumber(mkPDFNumber)
                                                )
import           Pdf.Document.Collection        ( PDFObjects
                                                , EncodedObjects
                                                , ObjectOffsets
                                                , EncodedObject
                                                  ( EncodedObject
                                                  , eoBinaryData
                                                  )
                                                , encodeObject
                                                )
import           Pdf.Document.Document          ( toList
                                                , cSize
                                                )
import           Util.Number                    ( encodeIntToBytes
                                                , bytesNeededToEncode
                                                )
import           Util.Dictionary                ( mkDictionary )
import qualified Data.ByteString               as BS
import           Data.Maybe                     ( fromMaybe )

-- | Given a collection of encoded objects, calculates their offsets
calcOffsets :: Int -> EncodedObjects -> ObjectOffsets
calcOffsets startOffset = snd . calcOffset . sort . toList
 where
  calcOffset :: [EncodedObject] -> (Int, ObjectOffsets)
  calcOffset = foldl'
    (\(offset, offsets) (EncodedObject number objectLength _) ->
      (offset + objectLength, IM.insert number offset offsets)
    )
    (startOffset, IM.empty)

{- |
Given the objects offsets, return the range of numbers of the objects.
-}
objectsNumberRange :: ObjectOffsets -> (Int, Int)
objectsNumberRange offsets = if IM.null offsets
  then (0, 0)
  else IM.foldlWithKey'
    (\(mini, maxi) objNum _ -> (min objNum mini, max objNum maxi))
    (maxBound, minBound)
    offsets

{- |
Given a collection of encoded objects, generates an old format XRef table
-}
xrefTable
  :: Int -- ^ Absolute offset of the first object
  -> EncodedObjects -- ^ The encoded objects to reference
  -> PDFObject -- ^ The old format XRef table
xrefTable startOffset objects | objects == mempty = PDFXRef []
                              | otherwise         = PDFXRef [xrefSubsection]
 where
  offsets  = calcOffsets startOffset objects
  numRange = objectsNumberRange offsets
  entries =
    [ maybe freeEntry (`inUseEntry` 0) (IM.lookup index offsets)
    | index <- range numRange
    ]
  xrefSubsection = XRefSubsection (fst numRange) (rangeSize numRange) entries

-- | Given a collection of encoded objects, generates a cross-reference stream.
xrefStreamTable
  :: Int -- ^ Number of the object to create
  -> Int -- ^ Absolute offset of the first object
  -> EncodedObjects -- ^ The encoded objects to reference
  -> PDFObject -- ^ The new format XRef table
xrefStreamTable number startOffset objects
  | objects == mempty = PDFXRef []
  | otherwise         = PDFXRefStream number 0 xrefDictionary stream

 where
  offsets                   = calcOffsets startOffset objects
  (firstNumber, lastNumber) = objectsNumberRange offsets
  numberByteCount           = bytesNeededToEncode lastNumber
  lastNumberOffset = fromMaybe startOffset (IM.lookup lastNumber offsets)
  offsetByteCount           = bytesNeededToEncode lastNumberOffset
  offsetCount               = cSize objects

  xrefDictionary            = mkDictionary
    [ ("Type", PDFName "XRef")
    , ( "W"
      , mkPDFArray
        [ mkPDFNumber (1 :: Double)
        , mkPDFNumber offsetByteCount
        , mkPDFNumber numberByteCount
        ]
      )
    , ("Index", mkPDFArray [mkPDFNumber firstNumber, mkPDFNumber offsetCount])
    , ("Size" , mkPDFNumber (BS.length stream))
    ]

  stream = IM.foldrWithKey
    (\index offset stream' -> BS.concat
      [ "\x01"
      , encodeIntToBytes offsetByteCount offset
      , encodeIntToBytes numberByteCount index
      , stream'
      ]
    )
    ""
    offsets
