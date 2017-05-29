module Test.Parsers
       ( parsingSuite
       ) where

import Prelude
import Data.Either (Either(..))
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (equal)

import Parsers (StringParser, float, int, parseWith)

parsingSuite :: forall e. TestSuite e
parsingSuite =
  suite "Parsing" do
    test "integer parsing" do
      parsesSuccessfullyAs int "1234" 1234
      parsesSuccessfullyAs int "-42" (-42)
    test "float parsing" do
      parsesSuccessfullyAs float  "12.34"     12.34
      parsesSuccessfullyAs float "-42"      (-42.0)
      parsesSuccessfullyAs float "-42.4242" (-42.4242)

parsesSuccessfullyAs :: forall a e. Eq a => Show a => StringParser a -> String -> a -> Test e
parsesSuccessfullyAs parser input f = equal (Right f) (parseWith parser input)
     