module Util.ParserHelper
  ( parseFullCompleted
  , parseFailed
  , shouldBeParsedAs
  , shouldBeFullyParsed
  , shouldFail
  , testExpected
  , itWith
  )
where

import Control.Monad (unless)

import Data.Binary.Parser (ByteOffset, Get, parseDetail)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)

import Test.Hspec
    ( Expectation
    , HasCallStack
    , SpecWith
    , expectationFailure
    , it
    , shouldBe
    )

type ParseDetailResult :: Type -> Type
type ParseDetailResult a
  = Either (ByteString, ByteOffset, String) (ByteString, ByteOffset, a)

parseFullCompleted :: ParseDetailResult a -> Bool
parseFullCompleted (Right (remain, _, _)) = BS.length remain == 0
parseFullCompleted _                      = False

parseFailed :: ParseDetailResult a -> Bool
parseFailed (Left _) = True
parseFailed _        = False

shouldBeParsedAs
  :: (HasCallStack, Show a, Eq a) => ParseDetailResult a -> a -> Expectation
shouldBeParsedAs result@(Right (remain, _, parsed)) expected
  | BS.length remain /= 0 = expectationFailure
    ("could not be parsed " ++ show result)
  | otherwise = parsed `shouldBe` expected
shouldBeParsedAs result _ =
  expectationFailure ("could not be parsed " ++ show result)

shouldFail :: HasCallStack => ParseDetailResult a -> Expectation
shouldFail result = parseFailed result `shouldBe` True

shouldBeFullyParsed
  :: (HasCallStack, Show a) => ParseDetailResult a -> Expectation
shouldBeFullyParsed result = unless
  (parseFullCompleted result)
  (expectationFailure $ "could not be fully parsed " ++ show result)

testExpected
  :: (HasCallStack, Show a, Eq a) => Get a -> (ByteString, a) -> Expectation
testExpected parser (example, expected) = do
  let parsed = parseDetail parser example
  shouldBeFullyParsed parsed
  parsed `shouldBeParsedAs` expected

itWith :: (Show a, Eq a) => String -> Get a -> (ByteString, a) -> SpecWith ()
itWith message parser test@(example, _) =
  it (message ++ show example) (testExpected parser test)
