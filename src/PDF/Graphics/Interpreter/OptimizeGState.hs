module PDF.Graphics.Interpreter.OptimizeGState
  ( optimizeGState
  )
where

import Data.Logging (Logging)
import Data.PDF.Command (Command (Command), mkCommand)
import Data.PDF.ExtGState (mkExtGState)
import Data.PDF.GFXObject
  ( GFXObject (GFXName)
  , GSOperator (GSSetColourRenderingIntent, GSSetFlatnessTolerance, GSSetLineCap, GSSetLineDashPattern, GSSetLineJoin, GSSetLineWidth, GSSetMiterLimit, GSSetParameters, GSSetTextFont)
  )
import Data.PDF.PDFObject (PDFObject (PDFDictionary))
import Data.PDF.PDFWork (PDFWork, addAdditionalGState)
import Data.PDF.Program (Program)
import Data.PDF.Resource (resName)
import Data.Sequence (Seq (Empty, (:<|)), spanl, (<|))

isFactorizable :: Command -> Bool
isFactorizable (Command GSSetLineWidth _parameters)             = True
isFactorizable (Command GSSetLineCap _parameters)               = True
isFactorizable (Command GSSetLineJoin _parameters)              = True
isFactorizable (Command GSSetMiterLimit _parameters)            = True
isFactorizable (Command GSSetLineDashPattern _parameters)       = True
isFactorizable (Command GSSetColourRenderingIntent _parameters) = True
isFactorizable (Command GSSetTextFont _parameters)              = True
isFactorizable (Command GSSetFlatnessTolerance _parameters)     = True
isFactorizable _anyOtherCommand                                 = False

isNotFactorizable :: Command -> Bool
isNotFactorizable = not . isFactorizable

optimizeGState :: Logging m => Program -> PDFWork m Program
optimizeGState Empty = return Empty
optimizeGState commands@(command :<| _rest) =
  if isFactorizable command
    then do
      let (factorizables, rest) = spanl isFactorizable commands

      resource <- addAdditionalGState (PDFDictionary $ mkExtGState factorizables)
      let factorized = mkCommand GSSetParameters
                                  [GFXName (resName resource)]

      optimizeds <- optimizeGState rest
      return $ factorized <| optimizeds
    else do
      -- No GState command to factorize, find the next one.
      let (notFactorizables, rest) = spanl isNotFactorizable commands
      optimizeds <- optimizeGState rest
      return $ notFactorizables <> optimizeds
