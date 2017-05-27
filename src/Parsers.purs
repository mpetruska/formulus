module Parsers
       ( StringParser
       , englishLetter
       , int
       , float
       , anyStringNotContaining
       , parseWith
       ) where

import Prelude
import Data.Array (many, notElem, singleton, some)
import Data.Either (Either)
import Data.Char.Unicode (isDigit)
import Data.Int (round)
import Data.String (fromCharArray, toCharArray)
import Global (readFloat, readInt)
import Text.Parsing.Parser (Parser, ParseError, runParser)
import Text.Parsing.Parser.Combinators (option)
import Text.Parsing.Parser.String (char, oneOf, satisfy)

type StringParser = Parser String

englishLetter :: StringParser Char
englishLetter = oneOf $ toCharArray "abcdefghijklmnopqrstuvwxyz"

someDigits :: StringParser (Array Char)
someDigits = some (satisfy isDigit)

int :: StringParser Int
int = round <$> readInt 10 <$> fromCharArray <$> someDigits

float :: StringParser Number
float =
    readFloat <$> fromCharArray <$> floatChars
  where
    dot        = singleton <$> char '.'
    floatChars = (<>) <$> someDigits
                      <*> option [] ((<>) <$> dot <*> someDigits)

anyStringNotContaining :: Array Char -> StringParser String
anyStringNotContaining chars =
  fromCharArray <$> (many $ satisfy (flip notElem chars))

parseWith :: forall a. StringParser a -> String -> Either ParseError a
parseWith = flip runParser