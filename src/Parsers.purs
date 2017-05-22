module Parsers
       ( StringParser
       , englishLetter
       ) where

import Prelude
import Data.String (toCharArray)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (oneOf)

type StringParser = Parser String

englishLetter :: StringParser Char
englishLetter = oneOf $ toCharArray "abcdefghijklmnopqrstuvwxyz"
