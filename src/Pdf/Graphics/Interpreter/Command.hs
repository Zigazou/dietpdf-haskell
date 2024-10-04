module Pdf.Graphics.Interpreter.Command
  ( Command (Command)
  , mkCommand
  , optimizeCommand
  )
where

import Control.Monad.State (State)

import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Sequence (fromList)

import Pdf.Graphics.Interpreter.GraphicsState
    ( GraphicsState
    , applyGraphicsMatrixS
    , applyTextMatrixS
    , restoreStateS
    , saveStateS
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
    ( GFXObject (GFXNumber)
    , GSOperator (GSRestoreGS, GSSaveGS, GSSetCTM, GSSetTextMatrix)
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
optimizeCommand command = case operator of
  -- Save graphics state
  GSSaveGS -> saveStateS >> return command

  -- Restore graphics state
  GSRestoreGS -> restoreStateS >> return command

  -- Set current transformation matrix
  GSSetCTM -> case toList parameters of
    [GFXNumber a, GFXNumber b, GFXNumber c, GFXNumber d, GFXNumber e, GFXNumber f] -> do
      applyGraphicsMatrixS (TransformationMatrix a b c d e f)
      return command
    _otherParameters -> return command

  -- Set text transformation matrix
  GSSetTextMatrix -> case toList parameters of
    [GFXNumber a, GFXNumber b, GFXNumber c, GFXNumber d, GFXNumber e, GFXNumber f] -> do
      applyTextMatrixS (TransformationMatrix a b c d e f)
      return command
    _otherParameters -> return command

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
