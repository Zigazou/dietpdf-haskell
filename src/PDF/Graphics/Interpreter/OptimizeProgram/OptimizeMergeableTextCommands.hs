{-|
Optimization of consecutive text display commands in PDF graphics programs.

This module provides functionality to optimize PDF graphics programs by merging
consecutive text showing commands ('GSShowText') into a single command. This
reduces the number of operations and can improve rendering efficiency when
multiple text strings are displayed in sequence.
-}
module PDF.Graphics.Interpreter.OptimizeProgram.OptimizeMergeableTextCommands
  ( optimizeMergeableTextCommands
  ) where

import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
  ( GFXObject (GFXArray, GFXHexString, GFXString)
  , GSOperator (GSShowManyText, GSShowText)
  )
import Data.PDF.Program (Program)
import Data.Sequence (Seq (Empty, (:<|)), (<|))

{-|
Merge two text objects into one, combining their string contents.
-}
mergeObjects :: GFXObject -> GFXObject -> GFXObject
mergeObjects (GFXString s1) (GFXString s2)       = GFXString (s1 <> s2)
mergeObjects (GFXHexString s1) (GFXHexString s2) = GFXHexString (s1 <> s2)
mergeObjects (GFXArray arr1) (GFXArray arr2)     = GFXArray (arr1 <> arr2)
mergeObjects _object1 object2                    = object2

{-|
Optimize a graphics program by merging consecutive text display commands.

This function traverses a graphics program and combines any consecutive
'GSShowText' commands into a single command with concatenated text strings.
This optimization reduces the number of operations in the program and can
improve rendering efficiency.

An empty program returns an empty program. When two consecutive 'GSShowText'
commands are encountered, they are merged by concatenating their string
arguments. All other commands are preserved as-is.

The optimization is applied recursively throughout the entire program.

__Examples:__

@
-- Program with consecutive text commands
program = (Command GSShowText \"Hello\")
        <| (Command GSShowText \" World\")
        <| mempty

-- After optimization
result = optimizeMergeableTextCommands program
-- Result: (Command GSShowText \"Hello World\") <| mempty
@
-}
optimizeMergeableTextCommands :: Program -> Program
-- Base case: An empty program remains unchanged.
optimizeMergeableTextCommands Empty = mempty

-- Merge consecutive GSShowText commands.
optimizeMergeableTextCommands
  (   (Command GSShowText (string1 :<| Empty))
  :<| (Command GSShowText (string2 :<| Empty))
  :<| afterShowText
  ) = Command GSShowText (mergeObjects string1 string2 <| Empty)
  <| afterShowText

-- Merge consecutive GSShowManyText commands.
optimizeMergeableTextCommands
  (   (Command GSShowManyText (array1 :<| Empty))
  :<| (Command GSShowManyText (array2 :<| Empty))
  :<| afterShowManyText
  ) = Command GSShowManyText (mergeObjects array1 array2 <| Empty)
  <| afterShowManyText

-- Preserve other commands and continue optimization.
optimizeMergeableTextCommands (command :<| rest) =
    command <| optimizeMergeableTextCommands rest
