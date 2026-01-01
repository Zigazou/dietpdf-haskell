module Data.PDF.Resource (
  Resource (
    ResColorSpace,
    ResFont,
    ResXObject,
    ResExtGState,
    ResProperties,
    ResPattern,
    ResShading,
    ResProcSet
  ),
  toResource,
  createSet,
  rmap,
  resName,
  toNameBase,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.HasLength (HasLength (objectLength))
import Data.Kind (Type)
import Data.Set (Set)
import Data.Set qualified as Set

type Resource :: Type
data Resource
  = ResColorSpace !ByteString
  | ResFont !ByteString
  | ResXObject !ByteString
  | ResExtGState !ByteString
  | ResProperties !ByteString
  | ResPattern !ByteString
  | ResShading !ByteString
  | ResProcSet !ByteString
  deriving stock (Eq, Ord, Show)

-- | Get the name of a resource.
resName :: Resource -> ByteString
resName (ResColorSpace name) = name
resName (ResFont name)       = name
resName (ResXObject name)    = name
resName (ResExtGState name)  = name
resName (ResProperties name) = name
resName (ResPattern name)    = name
resName (ResShading name)    = name
resName (ResProcSet name)    = name

instance HasLength Resource where
  objectLength :: Resource -> Int
  objectLength = objectLength . resName

-- | Map a function over the name of a resource.
rmap :: (ByteString -> ByteString) -> Resource -> Resource
rmap func (ResColorSpace name) = ResColorSpace (func name)
rmap func (ResFont name)       = ResFont (func name)
rmap func (ResXObject name)    = ResXObject (func name)
rmap func (ResExtGState name)  = ResExtGState (func name)
rmap func (ResProperties name) = ResProperties (func name)
rmap func (ResPattern name)    = ResPattern (func name)
rmap func (ResShading name)    = ResShading (func name)
rmap func (ResProcSet name)    = ResProcSet (func name)

{-| Convert a resource type and name to a 'Resource'.

If the resource type is not `ColorSpace`, `Font`, `XObject`, `ExtGState`,
`Properties`, or `Pattern`, then `Nothing` is returned.
-}
toResource :: ByteString -> ByteString -> Maybe Resource
toResource "ColorSpace" name          = Just $ ResColorSpace name
toResource "Font" name                = Just $ ResFont name
toResource "XObject" name             = Just $ ResXObject name
toResource "ExtGState" name           = Just $ ResExtGState name
toResource "Properties" name          = Just $ ResProperties name
toResource "Pattern" name             = Just $ ResPattern name
toResource "Shading" name             = Just $ ResShading name
toResource "ProcSet" name             = Just $ ResProcSet name
toResource _unknownResourceType _name = Nothing

{-| Create a set of resources from a list of `Maybe Resource`s.

This function is meant to be used with the `toResource` function to create a set
of resources from a list of resource types and names.

If a resource is `Nothing`, it is ignored.
-}
createSet :: [Maybe Resource] -> Set Resource
createSet = foldr go mempty
 where
  go :: Maybe Resource -> Set Resource -> Set Resource
  go (Just resource) resources = Set.insert resource resources
  go Nothing resources         = resources

baseDigits :: ByteString
baseDigits = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

toNameBase :: Resource -> Int -> Resource
toNameBase resource value = rmap (const (toNameBase' value "")) resource
  where
    toNameBase' :: Int -> ByteString -> ByteString
    toNameBase' 0 "" = "0"
    toNameBase' 0 acc = acc
    toNameBase' n acc =
      let (quotient, remainder) = n `divMod` BS.length baseDigits
      in toNameBase' quotient (BS.index baseDigits remainder `BS.cons` acc)
