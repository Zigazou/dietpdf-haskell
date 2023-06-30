module Font.TrueType.Parser.ScalerType
  ( scalerTypeP
  ) where

import           Font.TrueType.ScalerType       ( ScalerType
                                                , toScalerType
                                                )

import           Data.Binary.Parser             ( Get )
import           Data.Binary.Get.Internal       ( getByteString )

scalerTypeP :: Get ScalerType
scalerTypeP = toScalerType <$> getByteString 4
