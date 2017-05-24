module Data.Formula
       ( Formula
       , prettyPrintFormula
       , compactPrintFormula
       , const
       , value
       , negate
       , add
       , subtract
       , multiply
       , divide
       , formulaParser
       , parseFormula
       , referencedIdentifiers
       , evaluateFormula
       ) where

import Prelude
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either)
import Data.Ring as R
import Data.Set (Set, union)
import Data.Set as S
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Text.Parsing.Parser (ParseError, fail, runParser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.String (char, oneOf, string)
import Text.Parsing.Parser.Token (GenLanguageDef(..), LanguageDef, TokenParser, digit, makeTokenParser)

import Data.Identifier (Identifier, getIdentifierRepresentation, identifierParser)
import Parsers (StringParser, englishLetter, float)
import Validators (Error)

data Formula = Const Number
             | Value Identifier
             | Negate Formula
             | Add Formula Formula
             | Subtract Formula Formula
             | Multiply Formula Formula
             | Divide Formula Formula

derive instance eqFormula :: Eq Formula
instance showFormula :: Show Formula where
  show (Const n)        = "Const "     <> show n
  show (Value i)        = "Value "     <> show i
  show (Negate f)       = "Negate ("   <> show f  <> ")"
  show (Add f1 f2)      = "Add ("      <> show f1 <> ") (" <> show f2 <> ")"
  show (Subtract f1 f2) = "Subtract (" <> show f1 <> ") (" <> show f2 <> ")"
  show (Multiply f1 f2) = "Multiply (" <> show f1 <> ") (" <> show f2 <> ")"
  show (Divide f1 f2)   = "Divide ("   <> show f1 <> ") (" <> show f2 <> ")"

prettyPrintFormula :: Formula -> String
prettyPrintFormula (Const n)        = show n
prettyPrintFormula (Value i)        = getIdentifierRepresentation i
prettyPrintFormula (Negate f)       | precedence(f) > 1 = "-(" <> prettyPrintFormula f <> ")"
                                    | otherwise         = "-"  <> prettyPrintFormula f
prettyPrintFormula (Add f1 f2)      = prettyPrintFormula f1 <> " + " <> prettyPrintFormula f2
prettyPrintFormula (Subtract f1 f2) = prettyPrintFormula f1 <> " - " <> prettyPrintFormula f2
prettyPrintFormula (Multiply f1 f2) | precedence(f1) > 2 && precedence(f2) > 2 = "(" <> prettyPrintFormula f1 <> ") * (" <> prettyPrintFormula f2 <> ")"
                                    | precedence(f1) > 2                       = "(" <> prettyPrintFormula f1 <> ") * "  <> prettyPrintFormula f2
                                    |                       precedence(f2) > 2 =        prettyPrintFormula f1 <>  " * (" <> prettyPrintFormula f2 <> ")"
                                    | otherwise                                =        prettyPrintFormula f1 <>  " * "  <> prettyPrintFormula f2
prettyPrintFormula (Divide f1 f2)   | precedence(f1) > 2 && precedence(f2) > 2 = "(" <> prettyPrintFormula f1 <> ") / (" <> prettyPrintFormula f2 <> ")"
                                    | precedence(f1) > 2                       = "(" <> prettyPrintFormula f1 <> ") / "  <> prettyPrintFormula f2
                                    |                       precedence(f2) > 2 =        prettyPrintFormula f1 <>  " / (" <> prettyPrintFormula f2 <> ")"
                                    | otherwise                                =        prettyPrintFormula f1 <>  " / "  <> prettyPrintFormula f2

compactPrintFormula :: Formula -> String
compactPrintFormula = prettyPrintFormula >>> replaceAll (Pattern " ") (Replacement "")

precedence :: Formula -> Int
precedence (Const _)      = 1
precedence (Value _)      = 1
precedence (Negate _)     = 2
precedence (Add _ _)      = 3
precedence (Subtract _ _) = 3
precedence (Multiply _ _) = 2
precedence (Divide _ _)   = 2

const :: Number -> Formula
const = Const

value :: Identifier -> Formula
value = Value

negate :: Formula -> Formula
negate = Negate

add :: Formula -> Formula -> Formula
add = Add

subtract :: Formula -> Formula -> Formula
subtract = Subtract

multiply :: Formula -> Formula -> Formula
multiply = Multiply

divide :: Formula -> Formula -> Formula
divide = Divide

referencedIdentifiers :: Formula -> Set Identifier
referencedIdentifiers (Const _)        = S.empty
referencedIdentifiers (Value i)        = S.singleton i
referencedIdentifiers (Negate f)       = referencedIdentifiers f
referencedIdentifiers (Add f1 f2)      = union (referencedIdentifiers f1) (referencedIdentifiers f2)
referencedIdentifiers (Subtract f1 f2) = union (referencedIdentifiers f1) (referencedIdentifiers f2)
referencedIdentifiers (Multiply f1 f2) = union (referencedIdentifiers f1) (referencedIdentifiers f2)
referencedIdentifiers (Divide f1 f2)   = union (referencedIdentifiers f1) (referencedIdentifiers f2)

evaluateFormula :: (Identifier -> Either Error Number) -> Formula -> Either Error Number
evaluateFormula resolve = eval
  where
    eval :: Formula -> Either Error Number
    eval (Const n)        = pure n
    eval (Value i)        = resolve i
    eval (Negate f)       = R.negate <$> eval f
    eval (Add f1 f2)      = (+) <$> (eval f1) <*> (eval f2)
    eval (Subtract f1 f2) = (-) <$> (eval f1) <*> (eval f2)
    eval (Multiply f1 f2) = (*) <$> (eval f1) <*> (eval f2)
    eval (Divide f1 f2)   = (/) <$> (eval f1) <*> (eval f2)

languageDef :: LanguageDef
languageDef =
  LanguageDef { commentStart:    ""
              , commentEnd:      ""
              , commentLine:     ""
              , nestedComments:  false
              , identStart:      char '_' <|> englishLetter
              , identLetter:     char '_' <|> englishLetter <|> digit
              , opStart:         oneOf ['+', '-', '*', '/']
              , opLetter:        fail "unexpected character"
              , reservedNames:   []
              , reservedOpNames: ["+", "-", "*", "/"]
              , caseSensitive:   true
              }

tokenParser :: TokenParser
tokenParser = makeTokenParser languageDef

formulaParser :: StringParser Formula
formulaParser = fix exprParser
  where
    lexeme         = tokenParser.lexeme
    whiteSpace     = tokenParser.whiteSpace
    parens         = tokenParser.parens
    
    constParser = Const <$> float
    valueParser = Value <$> identifierParser
    termParser p = parens p
               <|> constParser
               <|> valueParser
    formulaParser' p = whiteSpace *> termParser p <* whiteSpace
    
    operator op = try $ whiteSpace *> string op
    operatorTable =
      [ [ Prefix (operator "-" $> Negate)
        ]
      , [ Infix  (operator "*" $> Multiply) AssocLeft
        , Infix  (operator "/" $> Divide)   AssocLeft
        ]
      , [ Infix  (operator "+" $> Add)      AssocLeft
        , Infix  (operator "-" $> Subtract) AssocLeft
        ]
      ]
    
    exprParser p = buildExprParser operatorTable (formulaParser' p)

parseFormula :: String -> Either ParseError Formula
parseFormula input = runParser input formulaParser
