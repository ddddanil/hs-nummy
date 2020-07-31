module Main where

import Nummy.Prelude
import Test.Tasty       (TestTree, defaultMain, testGroup)

import Tests.Dimensions
import Tests.Units
import Tests.Quantities
import Tests.Expressions

import Nummy.Currency (newCurrencyCache)

main :: IO ()
main = do
  c <- newCurrencyCache
  defaultMain $
    testGroup "Tests"
    [ testDimensions
    , testUnits c
    , testQuantities c
    , testExpressions c
    ]


