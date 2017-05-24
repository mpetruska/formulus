module Test.Data.Arbitrary.Formula
       ( ArbitraryFormula
       , generatedFormula
       , genFormula
       ) where

import Prelude hiding (const)
import Data.NonEmpty (NonEmpty, (:|))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, choose, oneOf)

import Data.Formula (Formula, add, const, divide, multiply, negate, subtract, value)
import Test.Data.Arbitrary.Identifier (generatedIdentifier)

newtype ArbitraryFormula = ArbitraryFormula Formula

instance showArbitraryFormula :: Show ArbitraryFormula where
  show (ArbitraryFormula f) = show f

arbitraryFormula :: Formula -> ArbitraryFormula
arbitraryFormula = ArbitraryFormula

generatedFormula :: ArbitraryFormula -> Formula
generatedFormula (ArbitraryFormula f) = f

genConst :: Gen ArbitraryFormula
genConst = arbitraryFormula <$> const <$> choose 0.0 10000000000000.0

genValue :: Gen ArbitraryFormula
genValue = arbitraryFormula <$> value <$> generatedIdentifier <$> arbitrary

genNegate :: Gen ArbitraryFormula -> Gen ArbitraryFormula
genNegate gen = arbitraryFormula <$> negate <$> f
  where f = generatedFormula <$> gen

genAdd :: Gen ArbitraryFormula -> Gen ArbitraryFormula
genAdd gen = arbitraryFormula <$> (add <$> f <*> f)
  where f = generatedFormula <$> gen
  
genSubtract :: Gen ArbitraryFormula -> Gen ArbitraryFormula
genSubtract gen = arbitraryFormula <$> (subtract <$> f <*> f)
  where f = generatedFormula <$> gen

genMultiply :: Gen ArbitraryFormula -> Gen ArbitraryFormula
genMultiply gen = arbitraryFormula <$> (multiply <$> f <*> f)
  where f = generatedFormula <$> gen
  
genDivide :: Gen ArbitraryFormula -> Gen ArbitraryFormula
genDivide gen = arbitraryFormula <$> (divide <$> f <*> f)
  where f = generatedFormula <$> gen

endFormulae :: NonEmpty Array (Gen ArbitraryFormula)
endFormulae = genConst :| [ genConst
                          , genValue
                          ]

allFormulae :: Gen ArbitraryFormula -> NonEmpty Array (Gen ArbitraryFormula)
allFormulae gen = genConst :| [ genConst
                              , genValue
                              , genNegate gen
                              , genAdd gen
                              , genSubtract gen
                              , genMultiply gen
                              , genDivide gen
                              ]

genFormula' :: Int -> Gen ArbitraryFormula
genFormula' 0 = oneOf endFormulae
genFormula' n = oneOf $ allFormulae $ genFormula' (n - 1)

genFormula :: Gen ArbitraryFormula
genFormula = genFormula' 10

instance formulaArbitrary :: Arbitrary ArbitraryFormula where
  arbitrary = genFormula
