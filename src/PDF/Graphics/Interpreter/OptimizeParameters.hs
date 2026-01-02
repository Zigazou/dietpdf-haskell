{-|
Optimization of command parameters in PDF graphics

Optimizes command parameters by converting hex strings to regular strings and
reducing numeric precision throughout the parameter structure.

This module provides two complementary operations:

* Converting hex string representations to more compact string format
* Reducing the precision of floating-point numbers to a specified decimal places

Both operations recursively process nested structures (arrays and dictionaries),
ensuring consistent optimization throughout complex parameter values.
-}
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

{-|
Convert hex string representations to regular strings recursively.

Replaces all hex string objects with their corresponding regular string
equivalents throughout nested structures. This conversion is often more compact
in the output representation.

Processes recursively:

* Direct hex strings are converted to strings
* Hex strings within arrays are converted
* Hex strings within dictionary values are converted

Other object types are passed through unchanged.

@param object@ a GFX object potentially containing hex strings @return@ the same
object with all hex strings converted to regular strings
-}
convertHexString :: GFXObject -> GFXObject
convertHexString (GFXHexString hex) = GFXString (hexStringToString hex)
convertHexString (GFXArray objects) = GFXArray (convertHexString <$> objects)
convertHexString (GFXDictionary dictionary) =
  mkGFXDictionary (fmap convertHexValue (Map.toList dictionary))
 where
  convertHexValue :: (ByteString, GFXObject) -> (ByteString, GFXObject)
  convertHexValue (key, value) = (key, convertHexString value)
convertHexString otherObject = otherObject

{-|
Optimize command parameters by reducing precision and converting hex strings.

Applies two transformations to all parameters of a command:

* Reduces floating-point numeric precision to the specified number of decimal
  places, removing unnecessary fractional digits
* Converts hex string representations to regular strings for more compact output

The optimizations are applied compositionally, so a parameter undergoes both
transformations in sequence (precision reduction first, then hex conversion).

@param command@ the command whose parameters to optimize @param precision@ the
maximum number of decimal places for numeric values @return@ a new command with
optimized parameters
-}
optimizeParameters :: Command -> Int -> Command
optimizeParameters command precision =
  command { cParameters = convertHexString
                        . reducePrecision precision
                      <$> cParameters command
          }
