module Data.Worksheet
       ( Input
       , Calculation
       , WorksheetRow(..)
       , input
       , calculation
       , Worksheet
       , CalculationResult(..)
       , WorksheetRowResult(..)
       , WorksheetResults
       , runWorksheet
       , encodeWorksheet
       , decodeWorksheet
       ) where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.State (StateT, evalStateT, get, modify, put)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Array (fromFoldable, singleton)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (maybe)
import Data.String (joinWith)
import Data.Validation.Semigroup (unV)
import Global (decodeURIComponent, encodeURIComponent)
import Text.Parsing.Parser (ParseError, fail, parseErrorMessage)
import Text.Parsing.Parser.Combinators (sepBy, try)
import Text.Parsing.Parser.String (string)

import Data.Identifier (Identifier, getIdentifierRepresentation, identifier)
import Data.Formula (Formula, compactPrintFormula, evaluateFormula, parseFormula)
import Parsers (StringParser, anyStringNotContaining, float, int, parseWith)
import Validators (Error)

type Input =
  { label      :: String
  , identifier :: Identifier
  , value      :: Number
  }

type Calculation =
  { label      :: String
  , identifier :: Identifier
  , formula    :: Formula
  , precision  :: Int
  }

data WorksheetRow = Input Input
                  | Calculation Calculation

eqInput :: Input -> Input -> Boolean
eqInput i1 i2 =    i1.label      == i2.label
                && i1.identifier == i2.identifier
                && i1.value      == i2.value

eqCalculation :: Calculation -> Calculation -> Boolean
eqCalculation c1 c2 =    c1.label      == c2.label
                      && c1.identifier == c2.identifier
                      && c1.formula    == c2.formula
                      && c1.precision  == c2.precision

instance eqWorksheetRow :: Eq WorksheetRow where
  eq (Input i1)       (Input i2)       = eqInput i1 i2
  eq (Calculation c1) (Calculation c2) = eqCalculation c1 c2
  eq _                _                = false

showInput :: Input -> String
showInput i =    "{ label: "      <> show i.label
              <> ", identifier: " <> show i.identifier
              <> ", value: "      <> show i.value
              <> "}"

showCalculation :: Calculation -> String
showCalculation c =    "{ label: "      <> show c.label
                    <> ", identifier: " <> show c.identifier
                    <> ", formula: "    <> show c.formula
                    <> ", precision: "  <> show c.precision
                    <> "}"

instance showWorksheetRow :: Show WorksheetRow where
  show (Input i)       = "Input "       <> showInput i
  show (Calculation c) = "Calculation " <> showCalculation c

input :: Input -> WorksheetRow
input = Input

calculation :: Calculation -> WorksheetRow
calculation = Calculation

type Worksheet = Array WorksheetRow

data CalculationResult = Result String
                       | Error Error

instance eqCalculationResult :: Eq CalculationResult where
  eq (Result r1) (Result r2) = eq r1 r2
  eq (Error e1)  (Error e2)  = eq e1 e2
  eq _           _           = false

instance showCalculationResult :: Show CalculationResult where
  show (Result r) = "Result " <> r
  show (Error e)  = "Error " <> e

data WorksheetRowResult = Nothing
                        | CalculationResult CalculationResult

instance eqWorksheetRowResult :: Eq WorksheetRowResult where
  eq (Nothing)              (Nothing)              = true
  eq (CalculationResult r1) (CalculationResult r2) = eq r1 r2
  eq _                      _                      = false

instance showWorksheetRowResult :: Show WorksheetRowResult where
  show (Nothing)             = "Nothing"
  show (CalculationResult r) = "CalculationResult " <> show r

type WorksheetResults = Array WorksheetRowResult

type ValueTable = Map Identifier Number

type WorksheetComputation = StateT ValueTable (Writer WorksheetResults)

runWorksheet :: Worksheet -> WorksheetResults
runWorksheet =
  traverse_ runWorksheetRow >>> flip evalStateT empty >>> execWriter

runWorksheetRow :: WorksheetRow -> WorksheetComputation Unit
runWorksheetRow (Input i) = do
  modify $ insert i.identifier i.value
  tell [ Nothing ]
runWorksheetRow (Calculation c) = do
    table <- get
    either (Error >>> CalculationResult >>> singleton >>> tell)
           (\value -> do
              put $ insert c.identifier value table
              tell [ CalculationResult $ Result $ format value ])
           (eval table)
  where
    resolve :: ValueTable -> Identifier -> Either Error Number
    resolve table i = maybe (Left ("Undefined value " <> show i)) Right $ lookup i table
    
    format :: Number -> String
    format n = show n -- TODO: pretty print number based on precision and using thousand separator
    
    eval :: ValueTable -> Either Error Number
    eval table = evaluateFormula (resolve table) c.formula

encodeWorksheet :: Worksheet -> String
encodeWorksheet =
    map encodeWorksheetRow >>> joinWith ","
  where
    encode = encodeURIComponent
    encodeWorksheetRow :: WorksheetRow -> String
    encodeWorksheetRow (Input i) =
         "i"
      <> ":" <> encode i.label
      <> ":" <> encode (getIdentifierRepresentation i.identifier)
      <> ":" <> encode (show i.value)
    encodeWorksheetRow (Calculation c) =
         "c"
      <> ":" <> encode c.label
      <> ":" <> encode (getIdentifierRepresentation c.identifier)
      <> ":" <> encode (compactPrintFormula c.formula)
      <> ":" <> encode (show c.precision)

worksheetParser :: StringParser Worksheet
worksheetParser =
    fromFoldable <$> row `sepBy` string ","
  where
    inputRow :: StringParser WorksheetRow
    inputRow = try do
      _          <- string "i:"
      label      <- anyStringNotContaining [':']
      _          <- string ":"
      idString   <- anyStringNotContaining [':']
      identifier <- unV (joinWith ", " >>> fail) pure (identifier $ decodeURIComponent idString)
      _          <- string ":"
      v          <- float
      pure $ input { label:      decodeURIComponent label
                   , identifier: identifier
                   , value:      v
                   }
        
    calculationRow :: StringParser WorksheetRow
    calculationRow = try do
      _          <- string "c:"
      label      <- anyStringNotContaining [':']
      _          <- string ":"
      idString   <- anyStringNotContaining [':']
      identifier <- unV (joinWith ", " >>> fail) pure (identifier $ decodeURIComponent idString)
      _          <- string ":"
      fString    <- anyStringNotContaining [':']
      formula    <- either (parseErrorMessage >>> fail) pure (parseFormula $ decodeURIComponent fString)
      _          <- string ":"
      precision  <- int
      pure $ calculation { label:      decodeURIComponent label
                         , identifier: identifier
                         , formula:    formula
                         , precision:  precision
                         }
    
    row :: StringParser WorksheetRow
    row = inputRow <|> calculationRow

decodeWorksheet :: String -> Either ParseError Worksheet
decodeWorksheet = parseWith worksheetParser
