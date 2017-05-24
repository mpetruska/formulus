module Test.Data.Arbitrary.Identifier
       ( ArbitraryIdentifier
       , generatedIdentifier
       , genIdentifier
       , identifier
       ) where

import Prelude
import Data.Char (fromCharCode, toCharCode)
import Data.Either (Either(..), fromRight)
import Data.NonEmpty ((:|))
import Data.String (fromCharArray)
import Data.Validation.Semigroup (unV)
import Partial.Unsafe (unsafePartialBecause)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, chooseInt, oneOf)

import Data.Identifier as I

newtype ArbitraryIdentifier = ArbitraryIdentifier I.Identifier

instance showArbitraryIdentifier :: Show ArbitraryIdentifier where
  show (ArbitraryIdentifier i) = show i

arbitraryIdentifier :: I.Identifier -> ArbitraryIdentifier
arbitraryIdentifier = ArbitraryIdentifier

generatedIdentifier :: ArbitraryIdentifier -> I.Identifier
generatedIdentifier (ArbitraryIdentifier i) = i

underscore :: Gen Char
underscore = pure '_'

lowerCaseLetter :: Gen Char
lowerCaseLetter = fromCharCode <$> chooseInt (toCharCode 'a') (toCharCode 'z')

digit :: Gen Char
digit = fromCharCode <$> chooseInt (toCharCode '0') (toCharCode '9')

genIdentifierFirstChar :: Gen Char
genIdentifierFirstChar =
  oneOf $ underscore :| [ underscore
                        , lowerCaseLetter
                        ]

genIdentifierOtherChar :: Gen Char
genIdentifierOtherChar =
    oneOf $ underscore :| [ underscore
                          , lowerCaseLetter
                          , digit
                          ]

genIdentifier :: Gen ArbitraryIdentifier
genIdentifier = do
  first <- genIdentifierFirstChar
  rest <- arrayOf genIdentifierOtherChar
  pure $ arbitraryIdentifier $ identifier $ fromCharArray $ [first] <> rest

instance identifierArbitrary :: Arbitrary ArbitraryIdentifier where
  arbitrary = genIdentifier
  
identifier :: String -> I.Identifier
identifier x =
    unsafePartialBecause ("invalid identifier in test: " <> x) $ fromRight identifier'
  where
    identifier' = unV Left Right (I.identifier x)
