module Pdf.Graphics.Interpreter.TextState
  ( TextState (..)
  , defaultTextState
  )
where

import Data.Kind (Type)

import Pdf.Graphics.Interpreter.TransformationMatrix (TransformationMatrix)
import Data.ByteString qualified as BS

{- |
The text state is a collection of parameters that define the current page's text
context. The text state includes the following parameters:

* The character spacing
* The word spacing
* The horizontal scaling
* The leading
* The font
* The font size
* The rendering mode
* The rise
-}
type TextState :: Type
data TextState = TextState
  { tsCharacterSpacing  :: !Double
  , tsWordSpacing       :: !Double
  , tsHorizontalScaling :: !Double
  , tsLeading           :: !Double
  , tsFont              :: !BS.ByteString
  , tsFontSize          :: !Double
  , tsRenderingMode     :: !Int
  , tsRise              :: !Double
  , tsMatrix            :: !TransformationMatrix
  , tsScaleX            :: !Double
  , tsScaleY            :: !Double
  } deriving stock (Show, Eq)

{- |
The default text state is the initial text state of a page. It has the following
parameters:

* The character spacing is 0.0
* The word spacing is 0.0
* The horizontal scaling is 100.0
* The leading is 0.0
* The font is "Helvetica"
* The font size is 12.0
* The rendering mode is 0
* The rise is 0.0
* The text matrix is the identity matrix
-}
defaultTextState :: TextState
defaultTextState = TextState
  { tsCharacterSpacing  = 0.0
  , tsWordSpacing       = 0.0
  , tsHorizontalScaling = 1.0
  , tsLeading           = 0.0
  , tsFont              = "Helvetica"
  , tsFontSize          = 12.0
  , tsRenderingMode     = 0
  , tsRise              = 0.0
  , tsMatrix            = mempty
  , tsScaleX            = 1.0
  , tsScaleY            = 1.0
  }
