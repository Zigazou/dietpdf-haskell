module Pdf.Graphics.Optimize (optimizeGFX) where

import Data.Array (Array)
import Data.ByteString qualified as BS
import Data.ByteString.Search (indices)
import Data.Context (Context)
import Data.Fallible (FallibleT)
import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.Logging (Logging, sayComparisonF, sayF)
import Data.Sequence qualified as SQ

import Pdf.Graphics.Object
    ( GFXObject (GFXArray, GFXNumber, GFXOperator)
    , GSOperator (GSCubicBezierCurve, GSCubicBezierCurve1To, GSCubicBezierCurve2To, GSLineTo, GSMoveTo, GSMoveToNextLine, GSMoveToNextLineLP, GSRectangle, GSRestoreGS, GSSaveGS, GSSetCTM, GSSetLineCap, GSSetLineJoin, GSSetLineWidth, GSSetMiterLimit, GSSetNonStrockeColorN, GSSetNonStrokeCMYKColorspace, GSSetNonStrokeColor, GSSetNonStrokeColorspace, GSSetNonStrokeGrayColorspace, GSSetNonStrokeRGBColorspace, GSSetStrokeCMYKColorspace, GSSetStrokeColor, GSSetStrokeColorN, GSSetStrokeColorspace, GSSetStrokeGrayColorspace, GSSetStrokeRGBColorspace, GSUnknown)
    , separateGfx
    )
import Pdf.Graphics.Parser.Stream (gfxParse)

type GFXCommand :: Type
data GFXCommand = GFXCommand
  { gcOperator   :: !GSOperator
  , gcParameters :: !(Array GFXObject)
  }

type GFXState :: Type
data GFXState = GFXState
  { gsTransform :: Double
  , gsPrecision :: Integer
  } deriving stock (Eq, Show)

defaultState :: GFXState
defaultState = GFXState
  { gsTransform = 1.0
  , gsPrecision = 3
  }

applyTransform :: Double -> GFXState -> GFXState
applyTransform coeff state = state
  { gsTransform = newCoeff
  , gsPrecision = max 0 (round (logBase 10 newCoeff) + 2)
  }
  where newCoeff = gsTransform state * coeff

type GFXStateStack :: Type
type GFXStateStack = [GFXState]

saveState :: GFXStateStack -> GFXStateStack
saveState []               = [defaultState]
saveState (state : states) = state : state : states

restoreState :: GFXStateStack -> GFXStateStack
restoreState []           = [defaultState]
restoreState (_ : states) = states

getState :: GFXStateStack -> GFXState
getState []             = defaultState
getState (gfxState : _) = gfxState

transformState :: Double -> GFXStateStack -> GFXStateStack
transformState coeff (state : states) = applyTransform coeff state : states
transformState coeff []               = [applyTransform coeff defaultState]

type OperatorCategory :: Type
data OperatorCategory
  = GraphicsCoordinate
  | TextCoordinate
  | Color
  | OtherCategory
  deriving stock (Eq, Show)

category :: GSOperator -> OperatorCategory
category GSMoveTo                     = GraphicsCoordinate
category GSLineTo                     = GraphicsCoordinate
category GSSetLineWidth               = GraphicsCoordinate
category GSSetLineCap                 = GraphicsCoordinate
category GSSetLineJoin                = GraphicsCoordinate
category GSSetMiterLimit              = GraphicsCoordinate
category GSCubicBezierCurve           = GraphicsCoordinate
category GSCubicBezierCurve1To        = GraphicsCoordinate
category GSCubicBezierCurve2To        = GraphicsCoordinate
category GSRectangle                  = GraphicsCoordinate
category GSMoveToNextLine             = TextCoordinate
category GSMoveToNextLineLP           = TextCoordinate
category GSSetStrokeColorspace        = Color
category GSSetNonStrokeColorspace     = Color
category GSSetStrokeColor             = Color
category GSSetStrokeColorN            = Color
category GSSetNonStrokeColor          = Color
category GSSetNonStrockeColorN        = Color
category GSSetStrokeGrayColorspace    = Color
category GSSetNonStrokeGrayColorspace = Color
category GSSetStrokeRGBColorspace     = Color
category GSSetNonStrokeRGBColorspace  = Color
category GSSetStrokeCMYKColorspace    = Color
category GSSetNonStrokeCMYKColorspace = Color
category _                            = OtherCategory

collectCommands
  :: (Array GFXObject, Array GFXCommand)
  -> GFXObject
  -> (Array GFXObject, Array GFXCommand)
collectCommands (gfxObjects, gfxCommands) (GFXOperator operator) =
  (mempty, gfxCommands SQ.|> GFXCommand operator gfxObjects)
collectCommands (gfxObjects, gfxCommands) gfxObject =
  (gfxObjects SQ.|> gfxObject, gfxCommands)

parseCommands :: Array GFXObject -> Array GFXCommand
parseCommands objects =
  let (gfxObjects, gfxCommands) = foldl' collectCommands (mempty, mempty) objects
  in  if SQ.null gfxObjects
    then gfxCommands
    else gfxCommands SQ.|> GFXCommand (GSUnknown "") gfxObjects

extractObjects :: Array GFXCommand -> Array GFXObject
extractObjects =
  foldl' (
    \acc (GFXCommand operator objects) ->
      acc <> objects SQ.|> GFXOperator operator
  ) mempty

roundN :: Integer -> Double -> Double
roundN n x = fromIntegral (round (x * (10.0 ^ n)) :: Int) / (10.0 ^ n)

reducePrecision :: Integer -> GFXObject -> GFXObject
reducePrecision precision (GFXNumber value) =
  GFXNumber (roundN precision value)
reducePrecision precision (GFXArray items) =
  GFXArray (reducePrecision precision <$> items)
reducePrecision _anyPrecision gfxObject = gfxObject

optimizeCommand :: GFXState -> GFXCommand -> GFXCommand
optimizeCommand state command
  | category operator == GraphicsCoordinate =
    command { gcParameters = reducePrecision (gsPrecision state) <$> parameters }
  | category operator == TextCoordinate =
    command { gcParameters = reducePrecision (gsPrecision state) <$> parameters }
  | category operator == Color =
    command { gcParameters = reducePrecision 3 <$> parameters }
  | otherwise = command
  where
    operator   = gcOperator command
    parameters = gcParameters command

optimizeCommands :: Array GFXCommand -> Array GFXCommand
optimizeCommands = snd . foldl' optimizeWithState mempty
  where
    optimizeWithState
      :: (GFXStateStack, Array GFXCommand)
      -> GFXCommand
      -> (GFXStateStack, Array GFXCommand)
    optimizeWithState (stack, optimizeds) cmd@(GFXCommand GSSaveGS _) =
      ( saveState stack, optimizeds SQ.|> cmd)
    optimizeWithState (stack, optimizeds) cmd@(GFXCommand GSRestoreGS _) =
      ( restoreState stack, optimizeds SQ.|> cmd)
    optimizeWithState (stack, optimizeds) cmd@(GFXCommand GSSetCTM parameters) =
      case parameters of
        GFXNumber precision SQ.:<| _ ->
          ( transformState precision stack
          , optimizeds SQ.|> optimizeCommand (getState stack) cmd
          )
        _otherParameters      -> ( stack, optimizeds SQ.|> cmd)
    optimizeWithState (stack, optimizeds) cmd =
      ( stack
      , optimizeds SQ.|> optimizeCommand (getState stack) cmd
      )

optimizeGFX
  :: Logging m
  => Context
  -> BS.ByteString
  -> FallibleT m BS.ByteString
optimizeGFX context stream = if indices "/CIDInit" stream /= []
  then return stream
  else
    case gfxParse stream of
      Right SQ.Empty -> return stream

      Right objects -> do
        let optimizedStream = separateGfx
                            . extractObjects
                            . optimizeCommands
                            . parseCommands
                            $ objects

        sayComparisonF
          context
          "GFX stream optimization"
          (BS.length stream)
          (BS.length optimizedStream)

        return optimizedStream

      _error -> do
          sayF context "GFX stream could not be parsed"
          return stream
