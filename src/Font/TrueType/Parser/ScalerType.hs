module Font.TrueType.Parser.ScalerType
  ( scalerTypeP
  ) where

import Data.Binary.Get.Internal (getByteString)
import Data.Binary.Parser (Get)

import Font.TrueType.ScalerType (ScalerType, toScalerType)

scalerTypeP :: Get ScalerType
scalerTypeP = toScalerType <$> getByteString 4
