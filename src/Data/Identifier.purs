module Data.Identifier
       ( Identifier
       , identifier
       , defaultIdentifier
       , identifierParser
       , getIdentifierRepresentation
       ) where

import Prelude
import Control.Alt ((<|>))
import Data.Array (many, singleton)
import Data.Either (Either)
import Data.String (fromCharArray)
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags (noFlags)
import Text.Parsing.Parser.String (char)
import Text.Parsing.Parser.Token (digit)

import Parsers (StringParser, englishLetter)
import Validators (Validated, matches', toValidated')

newtype Identifier = Identifier String

instance eqIdentifier :: Eq Identifier where
  eq (Identifier a) (Identifier b) = a == b

instance ordIdentifier :: Ord Identifier where
  compare (Identifier a) (Identifier b) = compare a b

instance showIdentifier :: Show Identifier where
  show (Identifier a) = "Identifier " <> show a

identifier :: String -> Validated Identifier
identifier x =
  Identifier <$>
    matches' (toValidated' identifierRegex) "identifier format is not valid" x

defaultIdentifier :: Identifier
defaultIdentifier = Identifier "x"

identifierRegex :: Either String Regex
identifierRegex = regex "^[\\_a-z][\\_a-z0-9]*$" noFlags

identifierParser :: StringParser Identifier
identifierParser = do
  c  <- char '_' <|> englishLetter
  cs <- many $ char '_' <|> englishLetter <|> digit
  pure $ Identifier $ fromCharArray (singleton c <> cs)

getIdentifierRepresentation :: Identifier -> String
getIdentifierRepresentation (Identifier s) = s
