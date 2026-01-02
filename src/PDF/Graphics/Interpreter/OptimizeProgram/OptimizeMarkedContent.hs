{-|
Optimization of marked content sequences in PDF graphics programs

Optimizes PDF marked content sequences by removing duplicated consecutive
BeginMarkedContentSequencePL operators when they are identical and properly
nested with corresponding EndMarkedContentSequence operators.

Marked content sequences (BDC/BMC...EMC) are PDF structures for grouping
graphics operations. Consecutive identical marked content wrappers can be merged
to reduce PDF file size.
-}
module PDF.Graphics.Interpreter.OptimizeProgram.OptimizeMarkedContent
  ( optimizeMarkedContent
  ) where

import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
  (GSOperator (GSBeginMarkedContentSequencePL, GSEndMarkedContentSequence))
import Data.PDF.Program (Program)
import Data.Sequence (Seq (Empty, (:<|)), breakl, (<|))

{-|
Test if a command terminates a marked content sequence.

Checks whether the given command is an EndMarkedContentSequence (EMC) operator,
which closes a marked content region.
-}
isEndMarkedContentSequence :: Command -> Bool
isEndMarkedContentSequence (Command GSEndMarkedContentSequence _) = True
isEndMarkedContentSequence _anyOtherCommand                       = False

{-|
Optimize marked content sequences by removing redundant nesting.

Removes duplicated consecutive BeginMarkedContentSequencePL operators when they
have identical parameters. Specifically:

* Detects consecutive BeginMarkedContentSequencePL commands with the same
  parameters
* Matches the corresponding nested EndMarkedContentSequence operators
* Removes the redundant inner marked content pair, keeping only the outer
  wrapper
* Recursively processes remaining commands

This optimization reduces file size by eliminating unnecessary marked content
nesting without changing the semantic meaning of the graphics program.
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
