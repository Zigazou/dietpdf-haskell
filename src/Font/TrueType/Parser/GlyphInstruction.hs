module Font.TrueType.Parser.GlyphInstruction
  ( glyphInstructionP
  , glyphInstructionsP
  ) where

import Control.Monad (replicateM)

import Data.Binary.Parser (Get, getWord16be, getWord8, isEmpty)
import Data.Bits ((.&.))
import Data.Word (Word8)

import Font.TrueType.FontTable.GlyphInstruction
  ( DistanceType (DistanceTypeA, DistanceTypeB, DistanceTypeC, DistanceTypeD)
  , GlyphInstruction (GIAbsoluteValue, GIAdd, GIAdjustAngle, GIAlignPoints, GIAlignToReferencePoint, GIAnd, GICall, GICeiling, GIClearStack, GICopyIndex, GIDebugCall, GIDeltaExceptionC1, GIDeltaExceptionC2, GIDeltaExceptionC3, GIDeltaExceptionP1, GIDeltaExceptionP2, GIDeltaExceptionP3, GIDivide, GIDuplicateTopStackElement, GIElse, GIEndFunctionDefinition, GIEndIf, GIEqual, GIEven, GIFlipPoint, GIFlipRangeOff, GIFlipRangeOn, GIFloor, GIFreedomVector, GIFunctionDefinition, GIGetCoordinateProjected, GIGetInformation, GIGreaterThan, GIGreaterThanOrEqual, GIIfTest, GIInstructionDefinition, GIInstructionExecutionControl, GIInterpolatePoint, GIInterpolateUntouchedPoints, GIJumpRelative, GIJumpRelativeOnFalse, GIJumpRelativeOnTrue, GILessThan, GILessThanOrEqual, GILoopAndCallFunction, GIMaximum, GIMeasureDistance, GIMeasurePixelsPerEM, GIMeasurePointSize, GIMinimum, GIMoveDirectAbsolutePoint, GIMoveDirectRelativePoint, GIMoveIndexedElement, GIMoveIndirectAbsolutePoint, GIMoveIndirectRelativePoint, GIMovePointToIntersection, GIMoveStackIndirectRelativePoint, GIMultiply, GINegate, GINoRoundingOfValue, GINot, GINotEqual, GIOdd, GIOr, GIPop, GIProjectionVector, GIPushBytes, GIPushNBytes, GIPushNWords, GIPushWords, GIReadControlValueTableEntry, GIReadStore, GIRollTopThreeStackElements, GIRound, GIRoundDownToGrid, GIRoundOff, GIRoundToDoubleGrid, GIRoundToGrid, GIRoundToHalfGrid, GIRoundUpToGrid, GISSuperRound45Degrees, GIScanConversionControl, GIScanType, GISetAngleWeight, GISetControlValueTableCutIn, GISetCoordinateFromStack, GISetDeltaBase, GISetDeltaShift, GISetDualProjectionVectorToLine, GISetFlipBooleanOff, GISetFlipBooleanOn, GISetFreedomAndProjectionVectorsToCoordinateAxis, GISetFreedomVectorFromStack, GISetFreedomVectorToCoordinateAxis, GISetFreedomVectorToLine, GISetFreedomVectorToProjectionVector, GISetLoopVariable, GISetMinimumDistance, GISetProjectionVectorCoordinateAxis, GISetProjectionVectorFromStack, GISetProjectionVectorToLine, GISetReferencePoint0, GISetReferencePoint1, GISetReferencePoint2, GISetSingleWidth, GISetSingleWidthCutIn, GISetZonePointer0, GISetZonePointer1, GISetZonePointer2, GISetZonePointerS, GIShiftContourReferencePoint, GIShiftPointPixelAmount, GIShiftPointReferencePoint, GIShiftZoneReferencePoint, GIStackDepth, GISubtract, GISuperRound, GISwap, GIUntouchPoint, GIWriteControlValueTableFUnits, GIWriteControlValueTablePixelsUnits, GIWriteStore)
  , InterpolationDirection (InterpolateInXDirection, InterpolateInYDirection)
  , MinimumDistanceControl (DoNotKeepDistanceGreaterThanOrEqual, KeepDistanceGreaterThanOrEqual)
  , OutlineMode (MeasureInGridFittedOutline, MeasureInOriginalOutline)
  , PointProjection (UseCurrentPositionOfPointP, UsePositionOfPointPInOriginalOutline)
  , ReferencePoint (ReferencePoint1, ReferencePoint2)
  , ResetRP0 (DoNotResetRP0, ResetRP0)
  , RoundingMode (DoNotRoundValue, RoundValue)
  , SetRP0 (DoNotSetRP0ToP, SetRP0ToP)
  , VectorAxis (AxisX, AxisY)
  , VectorPosition (ParallelToLine, PerpendicularToLine)
  )

{-|
Parser for a single 'GlyphInstruction'.
-}
glyphInstructionP :: Get GlyphInstruction
glyphInstructionP = do
  opcode <- getWord8
  case opcode of
    -- GIPushNBytes
    0x40 -> do
      count <- getWord8
      bytes <- replicateM (fromIntegral count) getWord8
      return (GIPushNBytes bytes)

    -- GIPushNWords
    0x41 -> do
      count <- getWord8
      word16s <- replicateM (fromIntegral count) getWord16be
      return (GIPushNWords word16s)

    -- GIPushBytes (0xB0-0xB7)
    _opcode | opcode >= 0xB0 && opcode <= 0xB7 -> do
      let count = fromIntegral (opcode - 0xB0 + 1)
      bytes <- replicateM count getWord8
      return (GIPushBytes bytes)

    -- GIPushWords (0xB8-0xBF)
    _opcode | opcode >= 0xB8 && opcode <= 0xBF -> do
      let count = fromIntegral (opcode - 0xB8 + 1)
      word16s <- replicateM count getWord16be
      return (GIPushWords word16s)

    -- GIMoveDirectRelativePoint (0xC0-0xDF)
    _opcode | opcode >= 0xC0 && opcode <= 0xDF -> do
      let rp0 = if (opcode .&. 0x20) /= 0 then ResetRP0 else DoNotResetRP0
      let mdc = if (opcode .&. 0x10) /= 0 then KeepDistanceGreaterThanOrEqual else DoNotKeepDistanceGreaterThanOrEqual
      let rounding = if (opcode .&. 0x04) /= 0 then RoundValue else DoNotRoundValue
      let distance = decodeDistanceType (opcode .&. 0x03)
      return (GIMoveDirectRelativePoint rp0 mdc rounding distance)

    -- GIMoveIndirectRelativePoint (0xE0-0xFF)
    _opcode | opcode >= 0xE0 && opcode <= 0xFF -> do
      let rp0 = if (opcode .&. 0x20) /= 0 then SetRP0ToP else DoNotSetRP0ToP
      let mdc = if (opcode .&. 0x10) /= 0 then KeepDistanceGreaterThanOrEqual else DoNotKeepDistanceGreaterThanOrEqual
      let rounding = if (opcode .&. 0x04) /= 0 then RoundValue else DoNotRoundValue
      let distance = decodeDistanceType (opcode .&. 0x03)
      return (GIMoveIndirectRelativePoint rp0 mdc rounding distance)

    -- All other single-byte instructions
    0x7F -> return GIAdjustAngle
    0x64 -> return GIAbsoluteValue
    0x60 -> return GIAdd
    0x27 -> return GIAlignPoints
    0x3C -> return GIAlignToReferencePoint
    0x5A -> return GIAnd
    0x2B -> return GICall
    0x67 -> return GICeiling
    0x25 -> return GICopyIndex
    0x22 -> return GIClearStack
    0x4F -> return GIDebugCall
    0x73 -> return GIDeltaExceptionC1
    0x74 -> return GIDeltaExceptionC2
    0x75 -> return GIDeltaExceptionC3
    0x5D -> return GIDeltaExceptionP1
    0x71 -> return GIDeltaExceptionP2
    0x72 -> return GIDeltaExceptionP3
    0x24 -> return GIStackDepth
    0x62 -> return GIDivide
    0x20 -> return GIDuplicateTopStackElement
    0x59 -> return GIEndIf
    0x1B -> return GIElse
    0x2D -> return GIEndFunctionDefinition
    0x54 -> return GIEqual
    0x57 -> return GIEven
    0x2C -> return GIFunctionDefinition
    0x4E -> return GISetFlipBooleanOff
    0x4D -> return GISetFlipBooleanOn
    0x80 -> return GIFlipPoint
    0x82 -> return GIFlipRangeOff
    0x81 -> return GIFlipRangeOn
    0x66 -> return GIFloor
    0x46 -> return (GIGetCoordinateProjected UseCurrentPositionOfPointP)
    0x47 -> return (GIGetCoordinateProjected UsePositionOfPointPInOriginalOutline)
    0x88 -> return GIGetInformation
    0x0D -> return GIFreedomVector
    0x0C -> return GIProjectionVector
    0x52 -> return GIGreaterThan
    0x53 -> return GIGreaterThanOrEqual
    0x89 -> return GIInstructionDefinition
    0x58 -> return GIIfTest
    0x8E -> return GIInstructionExecutionControl
    0x39 -> return GIInterpolatePoint
    0x0F -> return GIMovePointToIntersection
    0x30 -> return (GIInterpolateUntouchedPoints InterpolateInYDirection)
    0x31 -> return (GIInterpolateUntouchedPoints InterpolateInXDirection)
    0x1C -> return GIJumpRelative
    0x79 -> return GIJumpRelativeOnFalse
    0x78 -> return GIJumpRelativeOnTrue
    0x2A -> return GILoopAndCallFunction
    0x50 -> return GILessThan
    0x51 -> return GILessThanOrEqual
    0x8B -> return GIMaximum
    0x49 -> return (GIMeasureDistance MeasureInGridFittedOutline)
    0x4A -> return (GIMeasureDistance MeasureInOriginalOutline)
    0x2E -> return (GIMoveDirectAbsolutePoint DoNotRoundValue)
    0x2F -> return (GIMoveDirectAbsolutePoint RoundValue)
    0x3E -> return (GIMoveIndirectAbsolutePoint DoNotRoundValue)
    0x3F -> return (GIMoveIndirectAbsolutePoint RoundValue)
    0x8C -> return GIMinimum
    0x26 -> return GIMoveIndexedElement
    0x4B -> return GIMeasurePixelsPerEM
    0x4C -> return GIMeasurePointSize
    0x3A -> return (GIMoveStackIndirectRelativePoint DoNotSetRP0ToP)
    0x3B -> return (GIMoveStackIndirectRelativePoint SetRP0ToP)
    0x63 -> return GIMultiply
    0x65 -> return GINegate
    0x55 -> return GINotEqual
    0x5C -> return GINot
    0x6C -> return (GINoRoundingOfValue DistanceTypeA)
    0x6D -> return (GINoRoundingOfValue DistanceTypeB)
    0x6E -> return (GINoRoundingOfValue DistanceTypeC)
    0x6F -> return (GINoRoundingOfValue DistanceTypeD)
    0x56 -> return GIOdd
    0x5B -> return GIOr
    0x21 -> return GIPop
    0x45 -> return GIReadControlValueTableEntry
    0x7D -> return GIRoundDownToGrid
    0x7A -> return GIRoundOff
    0x8A -> return GIRollTopThreeStackElements
    0x68 -> return (GIRound DistanceTypeA)
    0x69 -> return (GIRound DistanceTypeB)
    0x6A -> return (GIRound DistanceTypeC)
    0x6B -> return (GIRound DistanceTypeD)
    0x43 -> return GIReadStore
    0x3D -> return GIRoundToDoubleGrid
    0x18 -> return GIRoundToGrid
    0x19 -> return GIRoundToHalfGrid
    0x7C -> return GIRoundUpToGrid
    0x77 -> return GISSuperRound45Degrees
    0x7E -> return GISetAngleWeight
    0x85 -> return GIScanConversionControl
    0x8D -> return GIScanType
    0x48 -> return GISetCoordinateFromStack
    0x1D -> return GISetControlValueTableCutIn
    0x5E -> return GISetDeltaBase
    0x86 -> return (GISetDualProjectionVectorToLine ParallelToLine)
    0x87 -> return (GISetDualProjectionVectorToLine PerpendicularToLine)
    0x5F -> return GISetDeltaShift
    0x0B -> return GISetFreedomVectorFromStack
    0x04 -> return (GISetFreedomVectorToCoordinateAxis AxisY)
    0x05 -> return (GISetFreedomVectorToCoordinateAxis AxisX)
    0x08 -> return (GISetFreedomVectorToLine ParallelToLine)
    0x09 -> return (GISetFreedomVectorToLine PerpendicularToLine)
    0x0E -> return GISetFreedomVectorToProjectionVector
    0x34 -> return (GIShiftContourReferencePoint ReferencePoint1)
    0x35 -> return (GIShiftContourReferencePoint ReferencePoint2)
    0x32 -> return (GIShiftPointReferencePoint ReferencePoint1)
    0x33 -> return (GIShiftPointReferencePoint ReferencePoint2)
    0x38 -> return GIShiftPointPixelAmount
    0x36 -> return (GIShiftZoneReferencePoint ReferencePoint1)
    0x37 -> return (GIShiftZoneReferencePoint ReferencePoint2)
    0x17 -> return GISetLoopVariable
    0x1A -> return GISetMinimumDistance
    0x0A -> return GISetProjectionVectorFromStack
    0x02 -> return (GISetProjectionVectorCoordinateAxis AxisY)
    0x03 -> return (GISetProjectionVectorCoordinateAxis AxisX)
    0x06 -> return (GISetProjectionVectorToLine ParallelToLine)
    0x07 -> return (GISetProjectionVectorToLine PerpendicularToLine)
    0x76 -> return GISuperRound
    0x10 -> return GISetReferencePoint0
    0x11 -> return GISetReferencePoint1
    0x12 -> return GISetReferencePoint2
    0x1F -> return GISetSingleWidth
    0x1E -> return GISetSingleWidthCutIn
    0x61 -> return GISubtract
    0x00 -> return (GISetFreedomAndProjectionVectorsToCoordinateAxis AxisY)
    0x01 -> return (GISetFreedomAndProjectionVectorsToCoordinateAxis AxisX)
    0x23 -> return GISwap
    0x13 -> return GISetZonePointer0
    0x14 -> return GISetZonePointer1
    0x15 -> return GISetZonePointer2
    0x16 -> return GISetZonePointerS
    0x29 -> return GIUntouchPoint
    0x70 -> return GIWriteControlValueTableFUnits
    0x44 -> return GIWriteControlValueTablePixelsUnits
    0x42 -> return GIWriteStore
    unknownOpcode -> fail $ "Unknown glyph instruction opcode: " ++ show unknownOpcode
  where
    decodeDistanceType :: Word8 -> DistanceType
    decodeDistanceType 0x00           = DistanceTypeA
    decodeDistanceType 0x01           = DistanceTypeB
    decodeDistanceType 0x02           = DistanceTypeC
    decodeDistanceType 0x03           = DistanceTypeD
    decodeDistanceType _anyOtherValue = DistanceTypeA  -- fallback

{-|
Parser for a list of 'GlyphInstruction's.
-}
glyphInstructionsP :: Get [GlyphInstruction]
glyphInstructionsP = do
  empty <- isEmpty
  if empty
    then return []
    else do
      instruction <- glyphInstructionP
      rest <- glyphInstructionsP
      return (instruction : rest)
