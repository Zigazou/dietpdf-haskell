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

indentSpaces :: T.Text
indentSpaces = "  "

indent :: Int -> T.Text -> T.Text
indent level = T.append (T.replicate level indentSpaces)

indent' :: Show a => Int -> a -> T.Text
indent' level value = T.append (T.replicate level indentSpaces) (T.pack (show value))

tshow :: Show a => a -> T.Text
tshow = T.dropEnd 1 . T.drop 1 . T.pack . show

type Human :: Type -> Constraint
class Human a where
  human :: Int -> a -> T.Text

instance Human GSOperator where
  human :: Int -> GSOperator -> T.Text
  human level value = indent level $ (T.pack . drop 2 . show) value

instance Human GFXObjects where
  human :: Int -> GFXObjects -> T.Text
  human level objects = indent level $ T.concat
    [ "["
    , T.intercalate ", " (human 0 <$> toList objects)
    , "]"
    ]

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

instance Human Command where
  human :: Int -> Command -> T.Text
  human level (Command operator SQ.Empty) = human level operator
  human level (Command operator parameters) = indent level $ T.concat
    [ human 0 operator
    , "("
    , T.intercalate ", " (human 0 <$> toList parameters)
    , ")"
    ]

instance Human Program where
  human :: Int -> Program -> T.Text
  human indentLevel = T.concat . toList . snd . foldl' go (indentLevel, mempty)
    where
      go :: (Int, Seq T.Text) -> Command -> (Int, Seq T.Text)
      go (level, humanCode) command = case command of
        (Command GSSaveGS _)    -> (level + 1, humanCode SQ.:|> (human level command <> "\n"))
        (Command GSRestoreGS _) -> (level - 1, humanCode SQ.:|> (human (level - 1) command <> "\n"))
        (Command GSBeginText _) -> (level + 1, humanCode SQ.:|> (human level command <> "\n"))
        (Command GSEndText _)   -> (level - 1, humanCode SQ.:|> (human (level - 1) command <> "\n"))
        _anyOtherCommand        -> (level, humanCode SQ.:|> (human level command <> "\n"))
