module PDF.Graphics.Interpreter.OptimizeProgram.OptimizeMarkedContent
  ( optimizeMarkedContent
  ) where

import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
  (GSOperator (GSBeginMarkedContentSequencePL, GSEndMarkedContentSequence))
import Data.PDF.Program (Program)
import Data.Sequence (Seq (Empty, (:<|)), breakl, (<|))

isEndMarkedContentSequence :: Command -> Bool
isEndMarkedContentSequence (Command GSEndMarkedContentSequence _) = True
isEndMarkedContentSequence _anyOtherCommand                       = False

{- |
Remove duplicated consecutive operators.
-}
optimizeMarkedContent :: Program -> Program
optimizeMarkedContent Empty = mempty

optimizeMarkedContent
  (   bmc1@(Command GSBeginMarkedContentSequencePL _params1)
  :<| bmc2@(Command GSBeginMarkedContentSequencePL _params2)
  :<| afterBMC
  ) = if bmc1 == bmc2
       then case breakl isEndMarkedContentSequence afterBMC of
              ( beforeEMC,    emc@(Command GSEndMarkedContentSequence _params3)
                          :<| Command GSEndMarkedContentSequence _params4
                          :<| afterEMC) ->
                (bmc1 <| beforeEMC) <> (emc <| optimizeMarkedContent afterEMC)
              _anyOtherCase -> bmc1 <| bmc2 <| optimizeMarkedContent afterBMC
       else
        bmc1 <| bmc2 <| optimizeMarkedContent afterBMC

optimizeMarkedContent (command :<| rest) = command <| optimizeMarkedContent rest
