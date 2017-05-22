module Test.Data.Formula
       ( formulaTestSuite
       ) where

import Prelude hiding (add, const, negate)
import Data.Either (Either(..), fromRight)
import Data.Foldable (traverse_)
import Data.Number (infinity, isNaN)
import Data.Validation.Semigroup (unV)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert, equal)
import Partial.Unsafe (unsafePartial, unsafePartialBecause)

import Data.Formula (Formula, add, const, divide, evaluateFormula, multiply, negate, parseFormula, subtract, value)
import Data.Identifier as I
import Validators (Error)

formulaTestSuite :: forall e. TestSuite e
formulaTestSuite = do
  parsingSuite
  evaluateSuite

parsingSuite :: forall e. TestSuite e
parsingSuite =
  suite "Formula parsing" do
    test "integer" do
      "1"       `parsesSuccessfullyAs` const 1.0
      " 1"      `parsesSuccessfullyAs` const 1.0
      "1 "      `parsesSuccessfullyAs` const 1.0
      " 1 "     `parsesSuccessfullyAs` const 1.0
    test "float" do
      "1.0"     `parsesSuccessfullyAs` const 1.0
      " 1.0"    `parsesSuccessfullyAs` const 1.0
      "1.0 "    `parsesSuccessfullyAs` const 1.0
      " 1.0 "   `parsesSuccessfullyAs` const 1.0
    test "identifier" do
      "x"       `parsesSuccessfullyAs` value (identifier "x")
      " x"      `parsesSuccessfullyAs` value (identifier "x")
      "x "      `parsesSuccessfullyAs` value (identifier "x")
      " x "     `parsesSuccessfullyAs` value (identifier "x")
    test "negate" do
      "-0.1"    `parsesSuccessfullyAs` negate (const 0.1)
      " -0.1"   `parsesSuccessfullyAs` negate (const 0.1)
      "-0.1 "   `parsesSuccessfullyAs` negate (const 0.1)
      " -0.1 "  `parsesSuccessfullyAs` negate (const 0.1)
      "- 0.1"   `parsesSuccessfullyAs` negate (const 0.1)
      " - 0.1"  `parsesSuccessfullyAs` negate (const 0.1)
      "- 0.1 "  `parsesSuccessfullyAs` negate (const 0.1)
      " - 0.1 " `parsesSuccessfullyAs` negate (const 0.1)
    
    test "addition" do
      parsingBinaryOperationTest "+" add
    test "subtraction" do
      parsingBinaryOperationTest "-" subtract
    test "multiplication" do
      parsingBinaryOperationTest "*" multiply
    test "division" do
      parsingBinaryOperationTest "/" divide
    
    test "complex formula" do
      "-( 11 +x_is_here )* 1.3333/_y" `parsesSuccessfullyAs`
        divide
          (multiply (negate $ add (const 11.0) (value (identifier "x_is_here"))) (const 1.3333))
          (value (identifier "_y"))

evaluateSuite :: forall e. TestSuite e
evaluateSuite =
    suite "Formula evaluation" do
      test "constant" do
        equal (Right 42.0) $ eval (formula "42")
      test "simple calculation" do
        equal (Right 111.0) $ eval (formula "x + y + z")
      test "complex calulcation" do
        equal (Right 1.2) $ eval (formula "-( -9 +-x )* 1.2/y")
      test "undefined value" do
        equal (Left "unknown value 'x0'") $ eval (formula "  x * y  - x0 ")
      test "division by zero" do
        equal (Right infinity) $ eval (formula "z / (x - 1)")
      test "nan" do
        assert "Expected nan" $ isNaN (unsafePartial $ fromRight $ eval $ formula "inf / inf")
  where
    eval :: Formula -> Either Error Number
    eval = evaluateFormula resolve
    resolve :: I.Identifier -> Either Error Number
    resolve i | I.getIdentifierRepresentation i == "x"   = Right 1.0
              | I.getIdentifierRepresentation i == "y"   = Right 10.0
              | I.getIdentifierRepresentation i == "z"   = Right 100.0
              | I.getIdentifierRepresentation i == "inf" = Right infinity
              | otherwise                                = Left ("unknown value '" <> I.getIdentifierRepresentation i <> "'")

identifier :: String -> I.Identifier
identifier x = unsafePartialBecause ("invalid identifier in test: " <> x) $ fromRight identifier'
  where
    identifier' = unV Left Right (I.identifier x)

formula :: String -> Formula
formula x = unsafePartialBecause ("invalid formula in test: " <> x) $ fromRight formula'
  where
    formula' = parseFormula x

parsesSuccessfullyAs :: forall e. String -> Formula -> Test e
parsesSuccessfullyAs input f = equal (Right f) (parseFormula input)

parsingBinaryOperationTest :: forall e. String -> (Formula -> Formula -> Formula) -> Test e
parsingBinaryOperationTest operator f = do
    traverse_ (flip parsesSuccessfullyAs (f e1 e2)) inputs1
    traverse_ (flip parsesSuccessfullyAs (f e2 e1)) inputs2
  where
    e1 = const 0.2
    e2 = value $ identifier "x0"
    inputs1 =
      [ " 0.2"  <> operator <>  "x0 "
      ,  "0.2 " <> operator <>  "x0"
      ,  "0.2"  <> operator <> " x0"
      ,  "0.2 " <> operator <> " x0"
      ]
    inputs2 =
      [ " x0"  <> operator <>  "0.2 "
      ,  "x0 " <> operator <>  "0.2"
      ,  "x0"  <> operator <> " 0.2"
      ,  "x0 " <> operator <> " 0.2"
      ]
