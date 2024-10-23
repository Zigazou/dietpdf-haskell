module PDF.Graphics.Interpreter.OptimizeParameters
  ( optimizeParameters
  ) where

import Data.PDF.Command (Command (cParameters))
import Data.PDF.GFXObject (GFXObject (GFXHexString, GFXString), reducePrecision)

import Util.String (hexStringToString)

convertHexString :: GFXObject -> GFXObject
convertHexString (GFXHexString hex) = GFXString (hexStringToString hex)
convertHexString otherObject        = otherObject

optimizeParameters :: Command -> Int -> Command
optimizeParameters command precision =
  command { cParameters = convertHexString
                        . reducePrecision precision
                      <$> cParameters command
          }
