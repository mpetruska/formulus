module Test.Data.Worksheet
       ( worksheetTestSuite
       ) where

import Prelude
import Data.Either (Either(..))
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (equal)

import Data.Worksheet (CalculationResult(..), Worksheet, WorksheetRowResult(..), WorksheetResults, input, calculation, decodeWorksheet, encodeWorksheet, runWorksheet)
import Test.Data.Arbitrary.Identifier (identifier)
import Test.Data.Arbitrary.Formula (formula)

worksheetTestSuite :: forall e. TestSuite e
worksheetTestSuite =
  suite "Worksheet" do
  
    test "encoding" do
      encodedSimpleWorksheet `equal` (encodeWorksheet simpleWorksheet)
    
    test "decoding" do
      (Right simpleWorksheet) `equal` (decodeWorksheet encodedSimpleWorksheet)
    
    test "run calculations" do
      encodedSimpleWorksheet                `calculatesAs` [ Nothing, CalculationResult (Result "53.0") ]
      "i:x:x:26,c:y:y:x%2Fx:0"              `calculatesAs` [ Nothing, CalculationResult (Result  "1.0") ]
      "i:x:x:12,i:y:y:34,c:z:z:100*x%2By:0" `calculatesAs` [ Nothing, Nothing, CalculationResult (Result "1234.0") ]

simpleWorksheet :: Worksheet
simpleWorksheet =
  [ input       { label: "label1",                       identifier: identifier "x",  value: 42.0 }
  , calculation { label: "label:with:colons,and,commas", identifier: identifier "_y", formula: formula "x + 11", precision: 2 }
  ]

encodedSimpleWorksheet :: String
encodedSimpleWorksheet = "i:label1:x:42.0,c:label%3Awith%3Acolons%2Cand%2Ccommas:_y:x%2B11.0:2"

calculatesAs :: forall e. String -> WorksheetResults -> Test e
calculatesAs encoded expected =
  (Right expected) `equal` (runWorksheet <$> decodeWorksheet encoded)