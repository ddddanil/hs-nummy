module Main where

import Nummy.Prelude
import Test.Tasty       (TestTree, defaultMain, testGroup)

import Tests.Dimensions
import Tests.Units
import Tests.Quantities
import Tests.Expressions


main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [ testDimensions
  , testUnits
  , testQuantities
  , testExpressions
  ]


