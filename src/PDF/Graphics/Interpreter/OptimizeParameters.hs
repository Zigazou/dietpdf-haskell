module PDF.Graphics.Interpreter.OptimizeParameters
  ( optimizeParameters
  , convertHexString
  ) where

import Data.ByteString (ByteString)
import Data.Map qualified as Map
import Data.PDF.Command (Command (cParameters))
import Data.PDF.GFXObject
  ( GFXObject (GFXArray, GFXDictionary, GFXHexString, GFXString)
  , mkGFXDictionary
  , reducePrecision
  )

import Util.String (hexStringToString)

convertHexString :: GFXObject -> GFXObject
convertHexString (GFXHexString hex) = GFXString (hexStringToString hex)
convertHexString (GFXArray objects) = GFXArray (convertHexString <$> objects)
convertHexString (GFXDictionary dictionary) =
  mkGFXDictionary (fmap convertHexValue (Map.toList dictionary))
 where
  convertHexValue :: (ByteString, GFXObject) -> (ByteString, GFXObject)
  convertHexValue (key, value) = (key, convertHexString value)
convertHexString otherObject = otherObject

optimizeParameters :: Command -> Int -> Command
optimizeParameters command precision =
  command { cParameters = convertHexString
                        . reducePrecision precision
                      <$> cParameters command
          }
