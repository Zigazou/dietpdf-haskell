module Pdf.Graphics.Interpreter.Command
  ( Command (Command)
  , mkCommand
  , optimizeCommand
  )
where

import Control.Monad.State (State)

import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Sequence (fromList, Seq ((:<|), Empty))

import Pdf.Graphics.Interpreter.GraphicsState
    ( GraphicsState
    , applyGraphicsMatrixS
    , applyTextMatrixS
    , restoreStateS
    , saveStateS
    , setFontS
    , setHorizontalScalingS
    , setTextRiseS
    , usefulColorPrecisionS
    , usefulGraphicsPrecisionS
    , usefulTextPrecisionS
    )
import Pdf.Graphics.Interpreter.OperatorCategory
    ( OperatorCategory (ClippingPathOperator, ColorOperator, PathConstructionOperator, PathPaintingOperator, TextPositioningOperator, TextShowingOperator, TextStateOperator, Type3FontOperator)
    , category
    )
import Pdf.Graphics.Interpreter.TransformationMatrix
    ( TransformationMatrix (TransformationMatrix)
    )
import Pdf.Graphics.Object
    ( GFXObject (GFXName, GFXNumber)
    , GSOperator (GSRestoreGS, GSSaveGS, GSSetCTM, GSSetHorizontalScaling, GSSetTextFont, GSSetTextMatrix, GSSetTextRise)
    , reducePrecision
    )
import Pdf.Graphics.Objects (Objects)

{- |
The 'Command' type represents a graphics state operator and its parameters.
-}
type Command :: Type
data Command = Command
  { cOperator   :: !GSOperator
  , cParameters :: !Objects
  }
  deriving stock (Eq, Show)

mkCommand :: GSOperator -> [GFXObject] -> Command
mkCommand = (. fromList) . Command

optimizeParameters :: Command -> Int -> Command
optimizeParameters command precision =
  command { cParameters = reducePrecision precision <$> cParameters command}

{- |
The 'optimizeCommand' function takes a 'GraphicsState' and a 'Command' and
returns an optimized 'Command'.
-}
optimizeCommand :: Command -> State GraphicsState Command
optimizeCommand command = case (operator, parameters) of
  -- Save graphics state
  (GSSaveGS, _params) -> saveStateS >> return command

  -- Restore graphics state
  (GSRestoreGS, _params) -> restoreStateS >> return command

  -- Set current transformation matrix
  (GSSetCTM, GFXNumber a :<| GFXNumber b :<| GFXNumber c :<| GFXNumber d :<| GFXNumber e :<| GFXNumber f :<| Empty) -> do
    applyGraphicsMatrixS (TransformationMatrix a b c d e f)
    usefulGraphicsPrecisionS <&> optimizeParameters command . (+ 2)

  -- Set text transformation matrix
  (GSSetTextMatrix, GFXNumber a :<| GFXNumber b :<| GFXNumber c :<| GFXNumber d :<| GFXNumber e :<| GFXNumber f :<| Empty) -> do
    applyTextMatrixS (TransformationMatrix a b c d e f)
    usefulTextPrecisionS <&> optimizeParameters command

  -- Set text font and size
  (GSSetTextFont, GFXName fontName :<| GFXNumber fontSize :<| Empty) -> do
    setFontS fontName fontSize
    usefulGraphicsPrecisionS <&> optimizeParameters command

  -- Set text horizontal scaling
  (GSSetHorizontalScaling, GFXNumber scaling :<| Empty) -> do
    setHorizontalScalingS scaling
    usefulGraphicsPrecisionS <&> optimizeParameters command

  -- Set text rise
  (GSSetTextRise, GFXNumber rise :<| Empty) -> do
    setTextRiseS rise
    return command

  -- Other operators
  _otherOperator -> case category operator of
    PathConstructionOperator -> usefulGraphicsPrecisionS <&> optimizeParameters command
    PathPaintingOperator     -> usefulGraphicsPrecisionS <&> optimizeParameters command
    ClippingPathOperator     -> usefulGraphicsPrecisionS <&> optimizeParameters command
    TextStateOperator        -> usefulTextPrecisionS <&> optimizeParameters command
    Type3FontOperator        -> usefulTextPrecisionS <&> optimizeParameters command
    TextPositioningOperator  -> usefulTextPrecisionS <&> optimizeParameters command
    TextShowingOperator      -> usefulTextPrecisionS <&> optimizeParameters command
    ColorOperator            -> usefulColorPrecisionS <&> optimizeParameters command

    _otherOperatorCategory   -> return command
 where
  operator   = cOperator command
  parameters = cParameters command
