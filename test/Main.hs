module Main where

import Protolude
import Data.String (String)
import Test.Tasty       (TestTree, defaultMain, testGroup, localOption, Timeout(Timeout))
import Test.Tasty.HUnit (testCase, (@?=), (@=?), Assertion, assertBool, assertEqual, assertFailure)
import Test.Tasty.ExpectedFailure (expectFail, expectFailBecause)
import Text.Parsec hiding (parseTest)
import Text.Parsec.String

import Tests.Dimensions
import Tests.Units
import Tests.Quantities


main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ testDimensions
  , testUnits
  , testQuantities
  ]


