module PDF.Graphics.Interpreter.OptimizeParameters
  ( optimizeParameters
  ) where


import Data.PDF.Command (Command (cParameters))
import Data.PDF.GFXObject (reducePrecision)


optimizeParameters :: Command -> Int -> Command
optimizeParameters command precision =
  command { cParameters = reducePrecision precision <$> cParameters command}
