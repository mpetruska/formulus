module Data.Worksheet
       ( Input
       , Calculation
       , WorksheetRow
       , input
       , calculation
       , CalculationResult(..)
       , WorksheetRowResult(..)
       , WorksheetResults
       , runWorksheet
       ) where

import Prelude
import Control.Monad.State (StateT, evalStateT, get, modify)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (maybe)

import Data.Identifier (Identifier)
import Data.Formula (Formula, evaluateFormula)
import Validators (Error)

type Input =
  { identifier :: Identifier
  , value      :: Number
  }

type Calculation =
  { identifier :: Identifier
  , formula    :: Formula
  , precision  :: Int
  }

data WorksheetRow = Input Input
                  | Calculation Calculation

input :: Input -> WorksheetRow
input = Input

calculation :: Calculation -> WorksheetRow
calculation = Calculation

type Worksheet = Array WorksheetRow

data CalculationResult = Result String
                       | Error Error

instance showCalculationResult :: Show CalculationResult where
  show (Result r) = "Result " <> r
  show (Error e)  = "Error " <> e

data WorksheetRowResult = Nothing
                        | CalculationResult CalculationResult

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
  tell [Nothing]
  
runWorksheetRow (Calculation c) = do
    table <- get
    tell [ eval table ]
  where
    resolve :: ValueTable -> Identifier -> Either Error Number
    resolve table i = maybe (Left ("Undefined value " <> show i)) Right $ lookup i table
    
    format :: Number -> String
    format n = show n -- TODO: pretty print number based on precision and using thousand separator
    
    eval :: ValueTable -> WorksheetRowResult
    eval table = CalculationResult $ either Error (format >>> Result) $ evaluateFormula (resolve table) c.formula
