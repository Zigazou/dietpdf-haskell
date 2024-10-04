module Pdf.Graphics.Objects (Objects) where

import Data.Array (Array)
import Data.Kind (Type)

import Pdf.Graphics.Object (GFXObject)

type Objects :: Type
type Objects = Array GFXObject
