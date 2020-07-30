module Main where

import Protolude
import Test.Tasty       (TestTree, defaultMain, testGroup)

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


