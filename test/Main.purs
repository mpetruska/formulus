module Test.Main where

import Prelude
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

import Test.Data.Formula (formulaTestSuite)
import Test.Data.Worksheet (worksheetTestSuite)

main :: forall e. Eff ( console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR, random :: RANDOM | e) Unit
main = runTest do
  formulaTestSuite
  worksheetTestSuite
