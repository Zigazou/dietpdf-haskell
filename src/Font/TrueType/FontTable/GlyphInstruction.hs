module Font.TrueType.FontTable.GlyphInstruction
  ( GlyphInstruction (GIAbsoluteValue, GIAdd, GIAdjustAngle, GIAlignPoints, GIAlignToReferencePoint, GIAnd, GICall, GICeiling, GIClearStack, GICopyIndex, GIDebugCall, GIDeltaExceptionC1, GIDeltaExceptionC2, GIDeltaExceptionC3, GIDeltaExceptionP1, GIDeltaExceptionP2, GIDeltaExceptionP3, GIDivide, GIDuplicateTopStackElement, GIElse, GIEndFunctionDefinition, GIEndIf, GIEqual, GIEven, GIFlipPoint, GIFlipRangeOff, GIFlipRangeOn, GIFloor, GIFreedomVector, GIFunctionDefinition, GIGetCoordinateProjected, GIGetInformation, GIGreaterThan, GIGreaterThanOrEqual, GIIfTest, GIInstructionDefinition, GIInstructionExecutionControl, GIInterpolatePoint, GIInterpolateUntouchedPoints, GIJumpRelative, GIJumpRelativeOnFalse, GIJumpRelativeOnTrue, GILessThan, GILessThanOrEqual, GILoopAndCallFunction, GIMaximum, GIMeasureDistance, GIMeasurePixelsPerEM, GIMeasurePointSize, GIMinimum, GIMoveDirectAbsolutePoint, GIMoveDirectRelativePoint, GIMoveIndexedElement, GIMoveIndirectAbsolutePoint, GIMoveIndirectRelativePoint, GIMovePointToIntersection, GIMoveStackIndirectRelativePoint, GIMultiply, GINegate, GINoRoundingOfValue, GINot, GINotEqual, GIOdd, GIOr, GIPop, GIProjectionVector, GIPushBytes, GIPushNBytes, GIPushNWords, GIPushWords, GIReadControlValueTableEntry, GIReadStore, GIRollTopThreeStackElements, GIRound, GIRoundDownToGrid, GIRoundOff, GIRoundToDoubleGrid, GIRoundToGrid, GIRoundToHalfGrid, GIRoundUpToGrid, GISSuperRound45Degrees, GIScanConversionControl, GIScanType, GISetAngleWeight, GISetControlValueTableCutIn, GISetCoordinateFromStack, GISetDeltaBase, GISetDeltaShift, GISetDualProjectionVectorToLine, GISetFlipBooleanOff, GISetFlipBooleanOn, GISetFreedomAndProjectionVectorsToCoordinateAxis, GISetFreedomVectorFromStack, GISetFreedomVectorToCoordinateAxis, GISetFreedomVectorToLine, GISetFreedomVectorToProjectionVector, GISetLoopVariable, GISetMinimumDistance, GISetProjectionVectorCoordinateAxis, GISetProjectionVectorFromStack, GISetProjectionVectorToLine, GISetReferencePoint0, GISetReferencePoint1, GISetReferencePoint2, GISetSingleWidth, GISetSingleWidthCutIn, GISetZonePointer0, GISetZonePointer1, GISetZonePointer2, GISetZonePointerS, GIShiftContourReferencePoint, GIShiftPointPixelAmount, GIShiftPointReferencePoint, GIShiftZoneReferencePoint, GIStackDepth, GISubtract, GISuperRound, GISwap, GIUntouchPoint, GIWriteControlValueTableFUnits, GIWriteControlValueTablePixelsUnits, GIWriteStore)
  , DistanceType(DistanceTypeA,DistanceTypeB,DistanceTypeC,DistanceTypeD)
  , PointProjection(UseCurrentPositionOfPointP,UsePositionOfPointPInOriginalOutline)
  , InterpolationDirection(InterpolateInYDirection,InterpolateInXDirection)
  , OutlineMode(MeasureInGridFittedOutline,MeasureInOriginalOutline)
  , RoundingMode(DoNotRoundValue,RoundValue)
  , ResetRP0(DoNotResetRP0,ResetRP0)
  , MinimumDistanceControl(DoNotKeepDistanceGreaterThanOrEqual,KeepDistanceGreaterThanOrEqual)
  , SetRP0(DoNotSetRP0ToP,SetRP0ToP)
  , VectorPosition(ParallelToLine,PerpendicularToLine)
  , VectorAxis(AxisY,AxisX)
  , ReferencePoint(ReferencePoint1,ReferencePoint2)
  , fromGlyphInstruction
  , fromGlyphInstructions
  ) where

import Data.Bits (Bits ((.&.)), shiftR, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Word (Word16, Word8)

type DistanceType :: Type
data DistanceType
  = DistanceTypeA
  | DistanceTypeB
  | DistanceTypeC
  | DistanceTypeD
  deriving stock (Eq, Show)

type PointProjection :: Type
data PointProjection
  = UseCurrentPositionOfPointP
  | UsePositionOfPointPInOriginalOutline
  deriving stock (Eq, Show)

type InterpolationDirection :: Type
data InterpolationDirection
  = InterpolateInYDirection
  | InterpolateInXDirection
  deriving stock (Eq, Show)

type OutlineMode :: Type
data OutlineMode
  = MeasureInGridFittedOutline
  | MeasureInOriginalOutline
  deriving stock (Eq, Show)

type RoundingMode :: Type
data RoundingMode
  = DoNotRoundValue
  | RoundValue
  deriving stock (Eq, Show)

type ResetRP0 :: Type
data ResetRP0
  = DoNotResetRP0
  | ResetRP0
  deriving stock (Eq, Show)

type MinimumDistanceControl :: Type
data MinimumDistanceControl
  = DoNotKeepDistanceGreaterThanOrEqual
  | KeepDistanceGreaterThanOrEqual
  deriving stock (Eq, Show)

type SetRP0 :: Type
data SetRP0
  = DoNotSetRP0ToP
  | SetRP0ToP
  deriving stock (Eq, Show)

type VectorPosition :: Type
data VectorPosition
  = ParallelToLine
  | PerpendicularToLine
  deriving stock (Eq, Show)

type VectorAxis :: Type
data VectorAxis
  = AxisY
  | AxisX
  deriving stock (Eq, Show)

type ReferencePoint :: Type
data ReferencePoint
  = ReferencePoint1
  | ReferencePoint2
  deriving stock (Eq, Show)

{-|
-}
type GlyphInstruction :: Type
data GlyphInstruction
  = GIPushNBytes [Word8]
  | GIPushNWords [Word16]
  | GIPushBytes [Word8]
  | GIPushWords [Word16]
  | GIAdjustAngle
  | GIAbsoluteValue
  | GIAdd
  | GIAlignPoints
  | GIAlignToReferencePoint
  | GIAnd
  | GICall
  | GICeiling
  | GICopyIndex
  | GIClearStack
  | GIDebugCall
  | GIDeltaExceptionC1
  | GIDeltaExceptionC2
  | GIDeltaExceptionC3
  | GIDeltaExceptionP1
  | GIDeltaExceptionP2
  | GIDeltaExceptionP3
  | GIStackDepth
  | GIDivide
  | GIDuplicateTopStackElement
  | GIEndIf
  | GIElse
  | GIEndFunctionDefinition
  | GIEqual
  | GIEven
  | GIFunctionDefinition
  | GISetFlipBooleanOff
  | GISetFlipBooleanOn
  | GIFlipPoint
  | GIFlipRangeOff
  | GIFlipRangeOn
  | GIFloor
  | GIGetCoordinateProjected !PointProjection
  | GIGetInformation
  | GIFreedomVector
  | GIProjectionVector
  | GIGreaterThan
  | GIGreaterThanOrEqual
  | GIInstructionDefinition
  | GIIfTest
  | GIInstructionExecutionControl
  | GIInterpolatePoint
  | GIMovePointToIntersection
  | GIInterpolateUntouchedPoints !InterpolationDirection
  | GIJumpRelative
  | GIJumpRelativeOnFalse
  | GIJumpRelativeOnTrue
  | GILoopAndCallFunction
  | GILessThan
  | GILessThanOrEqual
  | GIMaximum
  | GIMeasureDistance !OutlineMode
  | GIMoveDirectAbsolutePoint !RoundingMode
  | GIMoveDirectRelativePoint !ResetRP0
                              !MinimumDistanceControl
                              !RoundingMode
                              !DistanceType
  | GIMoveIndirectAbsolutePoint !RoundingMode
  | GIMinimum
  | GIMoveIndexedElement
  | GIMoveIndirectRelativePoint !SetRP0
                                !MinimumDistanceControl
                                !RoundingMode
                                !DistanceType
  | GIMeasurePixelsPerEM
  | GIMeasurePointSize
  | GIMoveStackIndirectRelativePoint !SetRP0
  | GIMultiply
  | GINegate
  | GINotEqual
  | GINot
  | GINoRoundingOfValue !DistanceType
  | GIOdd
  | GIOr
  | GIPop
  | GIReadControlValueTableEntry
  | GIRoundDownToGrid
  | GIRoundOff
  | GIRollTopThreeStackElements
  | GIRound !DistanceType
  | GIReadStore
  | GIRoundToDoubleGrid
  | GIRoundToGrid
  | GIRoundToHalfGrid
  | GIRoundUpToGrid
  | GISSuperRound45Degrees
  | GISetAngleWeight
  | GIScanConversionControl
  | GIScanType
  | GISetCoordinateFromStack
  | GISetControlValueTableCutIn
  | GISetDeltaBase
  | GISetDualProjectionVectorToLine !VectorPosition
  | GISetDeltaShift
  | GISetFreedomVectorFromStack
  | GISetFreedomVectorToCoordinateAxis !VectorAxis
  | GISetFreedomVectorToLine !VectorPosition
  | GISetFreedomVectorToProjectionVector
  | GIShiftContourReferencePoint !ReferencePoint
  | GIShiftPointReferencePoint !ReferencePoint
  | GIShiftPointPixelAmount
  | GIShiftZoneReferencePoint !ReferencePoint
  | GISetLoopVariable
  | GISetMinimumDistance
  | GISetProjectionVectorFromStack
  | GISetProjectionVectorCoordinateAxis !VectorAxis
  | GISetProjectionVectorToLine !VectorPosition
  | GISuperRound
  | GISetReferencePoint0
  | GISetReferencePoint1
  | GISetReferencePoint2
  | GISetSingleWidth
  | GISetSingleWidthCutIn
  | GISubtract
  | GISetFreedomAndProjectionVectorsToCoordinateAxis !VectorAxis
  | GISwap
  | GISetZonePointer0
  | GISetZonePointer1
  | GISetZonePointer2
  | GISetZonePointerS
  | GIUntouchPoint
  | GIWriteControlValueTableFUnits
  | GIWriteControlValueTablePixelsUnits
  | GIWriteStore
  deriving stock (Eq, Show)

{-|
Converts a list of Word16 values to a ByteString in big-endian order.
-}
packWord16bes :: [Word16] -> ByteString
packWord16bes = BS.pack . concatMap word16ToBytes
  where
    word16ToBytes :: Word16 -> [Word8]
    word16ToBytes w =
      [ fromIntegral (w `shiftR` 8)  -- High byte
      , fromIntegral (w .&. 0x00FF)  -- Low byte
      ]

fromGlyphInstruction :: GlyphInstruction -> ByteString
fromGlyphInstruction (GIPushNBytes bytes) =
  let count = fromIntegral (length bytes) :: Word8
  in BS.cons 0x40 $ BS.cons count $ BS.pack bytes
fromGlyphInstruction (GIPushNWords word16s) =
  let count = fromIntegral (length word16s) :: Word8
      wordBytes = packWord16bes word16s
  in  BS.cons 0x41 $ BS.cons count wordBytes
fromGlyphInstruction (GIPushBytes bytes) =
  let count = fromIntegral (length bytes) :: Word8
  in BS.cons (0xB0 + count - 1) $ BS.pack bytes
fromGlyphInstruction (GIPushWords word16s) =
  let count = fromIntegral (length word16s) :: Word8
      wordBytes = packWord16bes word16s
  in  BS.cons (0xB8 + count - 1) wordBytes
fromGlyphInstruction GIAdjustAngle = "\x7f"
fromGlyphInstruction GIAbsoluteValue = "\x64"
fromGlyphInstruction GIAdd = "\x60"
fromGlyphInstruction GIAlignPoints = "\x27"
fromGlyphInstruction GIAlignToReferencePoint = "\x3c"
fromGlyphInstruction GIAnd = "\x5a"
fromGlyphInstruction GICall = "\x2b"
fromGlyphInstruction GICeiling = "\x67"
fromGlyphInstruction GICopyIndex = "\x25"
fromGlyphInstruction GIClearStack = "\x22"
fromGlyphInstruction GIDebugCall = "\x4f"
fromGlyphInstruction GIDeltaExceptionC1 = "\x73"
fromGlyphInstruction GIDeltaExceptionC2 = "\x74"
fromGlyphInstruction GIDeltaExceptionC3 = "\x75"
fromGlyphInstruction GIDeltaExceptionP1 = "\x5d"
fromGlyphInstruction GIDeltaExceptionP2 = "\x71"
fromGlyphInstruction GIDeltaExceptionP3 = "\x72"
fromGlyphInstruction GIStackDepth = "\x24"
fromGlyphInstruction GIDivide = "\x62"
fromGlyphInstruction GIDuplicateTopStackElement = "\x20"
fromGlyphInstruction GIEndIf = "\x59"
fromGlyphInstruction GIElse = "\x1b"
fromGlyphInstruction GIEndFunctionDefinition = "\x2d"
fromGlyphInstruction GIEqual = "\x54"
fromGlyphInstruction GIEven = "\x57"
fromGlyphInstruction GIFunctionDefinition = "\x2c"
fromGlyphInstruction GISetFlipBooleanOff = "\x4e"
fromGlyphInstruction GISetFlipBooleanOn = "\x4d"
fromGlyphInstruction GIFlipPoint = "\x80"
fromGlyphInstruction GIFlipRangeOff = "\x82"
fromGlyphInstruction GIFlipRangeOn = "\x81"
fromGlyphInstruction GIFloor = "\x66"
fromGlyphInstruction (GIGetCoordinateProjected UseCurrentPositionOfPointP) = "\x46"
fromGlyphInstruction (GIGetCoordinateProjected UsePositionOfPointPInOriginalOutline) = "\x47"
fromGlyphInstruction GIGetInformation = "\x88"
fromGlyphInstruction GIFreedomVector = "\x0d"
fromGlyphInstruction GIProjectionVector = "\x0c"
fromGlyphInstruction GIGreaterThan = "\x52"
fromGlyphInstruction GIGreaterThanOrEqual = "\x53"
fromGlyphInstruction GIInstructionDefinition = "\x89"
fromGlyphInstruction GIIfTest = "\x58"
fromGlyphInstruction GIInstructionExecutionControl = "\x8e"
fromGlyphInstruction GIInterpolatePoint = "\x39"
fromGlyphInstruction GIMovePointToIntersection = "\x0f"
fromGlyphInstruction (GIInterpolateUntouchedPoints InterpolateInYDirection) = "\x30"
fromGlyphInstruction (GIInterpolateUntouchedPoints InterpolateInXDirection) = "\x31"
fromGlyphInstruction GIJumpRelative = "\x1c"
fromGlyphInstruction GIJumpRelativeOnFalse = "\x79"
fromGlyphInstruction GIJumpRelativeOnTrue = "\x78"
fromGlyphInstruction GILoopAndCallFunction = "\x2a"
fromGlyphInstruction GILessThan = "\x50"
fromGlyphInstruction GILessThanOrEqual = "\x51"
fromGlyphInstruction GIMaximum = "\x8b"
fromGlyphInstruction (GIMeasureDistance MeasureInGridFittedOutline) = "\x49"
fromGlyphInstruction (GIMeasureDistance MeasureInOriginalOutline) = "\x4a"
fromGlyphInstruction (GIMoveDirectAbsolutePoint DoNotRoundValue) = "\x2e"
fromGlyphInstruction (GIMoveDirectAbsolutePoint RoundValue) = "\x2f"
fromGlyphInstruction (GIMoveDirectRelativePoint rp0 mdc rounding distance) =
  let rp0Byte = case rp0 of
        DoNotResetRP0 -> 0x00 :: Word8
        ResetRP0      -> 0x20 :: Word8

      mdcByte = case mdc of
        DoNotKeepDistanceGreaterThanOrEqual -> 0x00 :: Word8
        KeepDistanceGreaterThanOrEqual      -> 0x10 :: Word8

      roundingByte = case rounding of
        DoNotRoundValue -> 0x00 :: Word8
        RoundValue      -> 0x04 :: Word8

      distanceByte = case distance of
        DistanceTypeA -> 0x00 :: Word8
        DistanceTypeB -> 0x01 :: Word8
        DistanceTypeC -> 0x02 :: Word8
        DistanceTypeD -> 0x03 :: Word8

      opcode = 0xc0 .|. rp0Byte .|. mdcByte .|. roundingByte .|. distanceByte
  in BS.singleton opcode
fromGlyphInstruction (GIMoveIndirectAbsolutePoint DoNotRoundValue) = "\x3e"
fromGlyphInstruction (GIMoveIndirectAbsolutePoint RoundValue) = "\x3f"
fromGlyphInstruction GIMinimum = "\x8c"
fromGlyphInstruction GIMoveIndexedElement = "\x26"
fromGlyphInstruction (GIMoveIndirectRelativePoint rp0 mdc rounding distance) =
  let rp0Byte = case rp0 of
        DoNotSetRP0ToP -> 0x00 :: Word8
        SetRP0ToP      -> 0x20 :: Word8

      mdcByte = case mdc of
        DoNotKeepDistanceGreaterThanOrEqual -> 0x00 :: Word8
        KeepDistanceGreaterThanOrEqual      -> 0x10 :: Word8

      roundingByte = case rounding of
        DoNotRoundValue -> 0x00 :: Word8
        RoundValue      -> 0x04 :: Word8

      distanceByte = case distance of
        DistanceTypeA -> 0x00 :: Word8
        DistanceTypeB -> 0x01 :: Word8
        DistanceTypeC -> 0x02 :: Word8
        DistanceTypeD -> 0x03 :: Word8

      opcode = 0xe0 .|. rp0Byte .|. mdcByte .|. roundingByte .|. distanceByte
  in BS.singleton opcode
fromGlyphInstruction GIMeasurePixelsPerEM = "\x4b"
fromGlyphInstruction GIMeasurePointSize = "\x4c"
fromGlyphInstruction (GIMoveStackIndirectRelativePoint DoNotSetRP0ToP) = "\x3a"
fromGlyphInstruction (GIMoveStackIndirectRelativePoint SetRP0ToP) = "\x3b"
fromGlyphInstruction GIMultiply = "\x63"
fromGlyphInstruction GINegate = "\x65"
fromGlyphInstruction GINotEqual = "\x55"
fromGlyphInstruction GINot = "\x5c"
fromGlyphInstruction (GINoRoundingOfValue DistanceTypeA) = "\x6c"
fromGlyphInstruction (GINoRoundingOfValue DistanceTypeB) = "\x6d"
fromGlyphInstruction (GINoRoundingOfValue DistanceTypeC) = "\x6e"
fromGlyphInstruction (GINoRoundingOfValue DistanceTypeD) = "\x6f"
fromGlyphInstruction GIOdd = "\x56"
fromGlyphInstruction GIOr = "\x5b"
fromGlyphInstruction GIPop = "\x21"
fromGlyphInstruction GIReadControlValueTableEntry = "\x45"
fromGlyphInstruction GIRoundDownToGrid = "\x7d"
fromGlyphInstruction GIRoundOff = "\x7a"
fromGlyphInstruction GIRollTopThreeStackElements = "\x8a"
fromGlyphInstruction (GIRound DistanceTypeA) = "\x8a"
fromGlyphInstruction (GIRound DistanceTypeB) = "\x8b"
fromGlyphInstruction (GIRound DistanceTypeC) = "\x8c"
fromGlyphInstruction (GIRound DistanceTypeD) = "\x8d"
fromGlyphInstruction GIReadStore = "\x43"
fromGlyphInstruction GIRoundToDoubleGrid = "\x3d"
fromGlyphInstruction GIRoundToGrid = "\x18"
fromGlyphInstruction GIRoundToHalfGrid = "\x19"
fromGlyphInstruction GIRoundUpToGrid = "\x7c"
fromGlyphInstruction GISSuperRound45Degrees = "\x77"
fromGlyphInstruction GISetAngleWeight = "\x7e"
fromGlyphInstruction GIScanConversionControl =  "\x85"
fromGlyphInstruction GIScanType = "\x8d"
fromGlyphInstruction GISetCoordinateFromStack = "\x48"
fromGlyphInstruction GISetControlValueTableCutIn = "\x1d"
fromGlyphInstruction GISetDeltaBase = "\x5e"
fromGlyphInstruction (GISetDualProjectionVectorToLine ParallelToLine) = "\x86"
fromGlyphInstruction (GISetDualProjectionVectorToLine PerpendicularToLine) = "\x87"
fromGlyphInstruction GISetDeltaShift = "\x5f"
fromGlyphInstruction GISetFreedomVectorFromStack = "\x0b"
fromGlyphInstruction (GISetFreedomVectorToCoordinateAxis AxisY) = "\x04"
fromGlyphInstruction (GISetFreedomVectorToCoordinateAxis AxisX) = "\x05"
fromGlyphInstruction (GISetFreedomVectorToLine ParallelToLine) = "\x08"
fromGlyphInstruction (GISetFreedomVectorToLine PerpendicularToLine) = "\x09"
fromGlyphInstruction GISetFreedomVectorToProjectionVector = "\x0e"
fromGlyphInstruction (GIShiftContourReferencePoint ReferencePoint1) = "\x34"
fromGlyphInstruction (GIShiftContourReferencePoint ReferencePoint2) = "\x35"
fromGlyphInstruction (GIShiftPointReferencePoint ReferencePoint1) = "\x32"
fromGlyphInstruction (GIShiftPointReferencePoint ReferencePoint2) = "\x33"
fromGlyphInstruction GIShiftPointPixelAmount = "\x38"
fromGlyphInstruction (GIShiftZoneReferencePoint ReferencePoint1) = "\x36"
fromGlyphInstruction (GIShiftZoneReferencePoint ReferencePoint2) = "\x37"
fromGlyphInstruction GISetLoopVariable = "\x17"
fromGlyphInstruction GISetMinimumDistance = "\x1a"
fromGlyphInstruction GISetProjectionVectorFromStack = "\x0a"
fromGlyphInstruction (GISetProjectionVectorCoordinateAxis AxisY) = "\x02"
fromGlyphInstruction (GISetProjectionVectorCoordinateAxis AxisX) = "\x03"
fromGlyphInstruction (GISetProjectionVectorToLine ParallelToLine) = "\x06"
fromGlyphInstruction (GISetProjectionVectorToLine PerpendicularToLine) = "\x07"
fromGlyphInstruction GISuperRound = "\x76"
fromGlyphInstruction GISetReferencePoint0 = "\x10"
fromGlyphInstruction GISetReferencePoint1 = "\x11"
fromGlyphInstruction GISetReferencePoint2 = "\x12"
fromGlyphInstruction GISetSingleWidth = "\x1f"
fromGlyphInstruction GISetSingleWidthCutIn = "\x1e"
fromGlyphInstruction GISubtract = "\x61"
fromGlyphInstruction (GISetFreedomAndProjectionVectorsToCoordinateAxis AxisY) = "\x00"
fromGlyphInstruction (GISetFreedomAndProjectionVectorsToCoordinateAxis AxisX) = "\x01"
fromGlyphInstruction GISwap = "\x23"
fromGlyphInstruction GISetZonePointer0 = "\x13"
fromGlyphInstruction GISetZonePointer1 = "\x14"
fromGlyphInstruction GISetZonePointer2 = "\x15"
fromGlyphInstruction GISetZonePointerS = "\x16"
fromGlyphInstruction GIUntouchPoint = "\x29"
fromGlyphInstruction GIWriteControlValueTableFUnits = "\x70"
fromGlyphInstruction GIWriteControlValueTablePixelsUnits = "\x44"
fromGlyphInstruction GIWriteStore = "\x42"

fromGlyphInstructions :: [GlyphInstruction] -> ByteString
fromGlyphInstructions = BS.concat . map fromGlyphInstruction