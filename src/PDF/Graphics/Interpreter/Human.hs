{-|
Human-readable text representation of PDF graphics objects

Provides human-readable text representations of PDF graphics objects, commands,
and programs. Converts low-level PDF structures (operators, numbers, strings,
dictionaries, arrays) into formatted text suitable for debugging and analysis.

The `Human` typeclass converts various PDF data structures to indented text with
appropriate formatting for visual inspection. Indentation levels are maintained
to show the structure of nested objects.
-}
module PDF.Graphics.Interpreter.Human
( human )
where

import Data.ByteString (ByteString)
import Data.Double.Conversion.Text (toShortest)
import Data.Foldable (foldl', toList)
import Data.Kind (Constraint, Type)
import Data.Map (assocs)
import Data.PDF.Command (Command (Command))
import Data.PDF.GFXObject
    ( GFXObject (GFXArray, GFXBool, GFXComment, GFXDictionary, GFXHexString, GFXInlineImage, GFXName, GFXNull, GFXNumber, GFXOperator, GFXReference, GFXString)
    , GSOperator (GSBeginText, GSEndText, GSRestoreGS, GSSaveGS)
    )
import Data.PDF.GFXObjects (GFXObjects)
import Data.PDF.Program (Program)
import Data.Sequence (Seq)
import Data.Sequence qualified as SQ
import Data.Text qualified as T

import Util.Dictionary (Dictionary)

{-|
Standard indentation unit for human-readable output.

Each indentation level adds this many spaces (2 spaces).
-}
indentSpaces :: T.Text
indentSpaces = "  "

{-|
Add indentation to text based on nesting level.

Prepends @level@ repetitions of @indentSpaces@ to the given text, creating
proper visual indentation for nested structures.
-}
indent :: Int -> T.Text -> T.Text
indent level = T.append (T.replicate level indentSpaces)

{-|
Add indentation and convert a showable value to text.

Combines @indent@ with @show@ to produce an indented text representation of any
type that implements @Show@. Used as a convenience for formatting simple values.
-}
indent' :: Show a => Int -> a -> T.Text
indent' level value = T.append (T.replicate level indentSpaces) (T.pack (show value))

{-|
Convert a showable value to text, removing surrounding quotes and parentheses.

Used internally to format strings and names by dropping the leading and trailing
quote characters that @show@ adds to string values.
-}
tshow :: Show a => a -> T.Text
tshow = T.dropEnd 1 . T.drop 1 . T.pack . show

{-|
Typeclass for converting PDF objects to human-readable text.

Provides a @human@ method that takes an indentation level and a value, and
produces a Text representation with appropriate formatting and indentation.

Instances exist for:

* @GSOperator@ - PDF graphics state operators (save, restore, etc.)
* @GFXObjects@ - Sequences of GFX objects as arrays
* @(ByteString, GFXObject)@ - Dictionary entries
* @Dictionary GFXObject@ - PDF dictionaries
* @GFXObject@ - All PDF object types
* @Command@ - PDF graphics commands
* @Program@ - Complete PDF graphics programs with nesting
-}
type Human :: Type -> Constraint
class Human a where
  human :: Int -> a -> T.Text

instance Human GSOperator where
  human :: Int -> GSOperator -> T.Text
  human level value = indent level $ (T.pack . drop 2 . show) value

{-|
Human representation of sequences of GFX objects.

Formats a sequence as a space-bracketed list of comma-separated objects, similar
to PDF array syntax. Each object is formatted without additional indentation.
-}
instance Human GFXObjects where
  human :: Int -> GFXObjects -> T.Text
  human level objects = indent level $ T.concat
    [ "["
    , T.intercalate ", " (human 0 <$> toList objects)
    , "]"
    ]

{-|
Human representation of a dictionary entry.

Formats a key-value pair as \"/key => value\". The key is a ByteString, rendered
as a string. The value is formatted using its Human instance.
-}
instance Human (ByteString, GFXObject) where
  human :: Int -> (ByteString, GFXObject) -> T.Text
  human level (name, value) = indent level $ T.concat
    [ "/"
    , T.pack (show name)
    , " => "
    , human 0 value
    ]

instance Human (Dictionary GFXObject) where
  human :: Int -> Dictionary GFXObject -> T.Text
  human level dict = indent level $ T.concat
    [ "{"
    , T.intercalate ", " (human 0 <$> assocs dict)
    , "}"
    ]

{-|
Human representation of a PDF object.

Formats different object types appropriately:

* Comments as \"%comment\"
* Numbers using shortest decimal representation
* Names as \"/name\"
* Strings with proper quoting
* Hex strings as \"#hexvalue\"
* References as \"@major.minor\"
* Arrays and dictionaries via their Human instances
* Booleans as \"True\" or \"False\"
* Null as \"null\"
* Inline images as their dictionary representation
* Operators via their Human instance
-}
instance Human GFXObject where
  human :: Int -> GFXObject -> T.Text
  human level (GFXComment value)          = indent level $ T.pack ('%':' ':show value)
  human level (GFXNumber value)           = indent level (toShortest value)
  human level (GFXName value)             = indent level $ "/" <> tshow value
  human level (GFXString value)           = indent level $ T.pack (show value)
  human level (GFXHexString value)        = indent level $ "#" <> tshow value
  human level (GFXReference major minor)  = indent level $ T.concat ["@", T.pack (show major), ".", T.pack (show minor)]
  human level (GFXArray objects)          = human level objects
  human level (GFXDictionary dict)        = human level dict
  human level (GFXBool value)             = indent' level value
  human level GFXNull                     = indent level "null"
  human level (GFXInlineImage dict _data) = human level dict
  human level (GFXOperator operator)      = human level operator

{-|
Human representation of a PDF graphics command.

Formats a command by indenting the operator and showing parameters in
parentheses. If there are no parameters, shows just the operator name. Multiple
parameters are comma-separated.
-}
instance Human Command where
  human :: Int -> Command -> T.Text
  human level (Command operator SQ.Empty) = human level operator
  human level (Command operator parameters) = indent level $ T.concat
    [ human 0 operator
    , "("
    , T.intercalate ", " (human 0 <$> toList parameters)
    , ")"
    ]

{-|
Human representation of a complete PDF graphics program.

Converts a sequence of graphics commands to indented text, maintaining proper
nesting levels for visual clarity. Indentation increases when entering scoped
blocks (save/restore pairs, text objects) and decreases when exiting them.

Each command is formatted on its own line with appropriate indentation based on
the current nesting level.
-}
instance Human Program where
  human :: Int -> Program -> T.Text
  human indentLevel = T.concat . toList . snd . foldl' go (indentLevel, mempty)
    where
      {-
      Helper function for folding over commands and tracking indentation.
      
      Adjusts indentation level based on operators that change scope:
      
      * @GSSaveGS@ increases indentation (entering graphics state block)
      * @GSRestoreGS@ decreases indentation (exiting graphics state block)
      * @GSBeginText@ increases indentation (entering text object)
      * @GSEndText@ decreases indentation (exiting text object)
      
      Appends each formatted command with a newline to the accumulated text.
      -}
      go :: (Int, Seq T.Text) -> Command -> (Int, Seq T.Text)
      go (level, humanCode) command = case command of
        (Command GSSaveGS _)    -> (level + 1, humanCode SQ.:|> (human level command <> "\n"))
        (Command GSRestoreGS _) -> (level - 1, humanCode SQ.:|> (human (level - 1) command <> "\n"))
        (Command GSBeginText _) -> (level + 1, humanCode SQ.:|> (human level command <> "\n"))
        (Command GSEndText _)   -> (level - 1, humanCode SQ.:|> (human (level - 1) command <> "\n"))
        _anyOtherCommand        -> (level, humanCode SQ.:|> (human level command <> "\n"))
