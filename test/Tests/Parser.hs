module Tests.Parser (unit', qu', testParse, TestType(..)) where

import Protolude
import Data.String (String)
import Test.Tasty       (TestTree, defaultMain, testGroup, localOption, Timeout(Timeout))
import Test.Tasty.HUnit (testCase, (@?=), (@=?), Assertion, (@?))
import Test.Tasty.ExpectedFailure (expectFail, expectFailBecause)
import Text.Parsec hiding (parseTest)
import Text.Parsec.String

import Nummy.Parser.Units
import Nummy.Parser
import Nummy.Metrology.Dimension
import Nummy.Metrology.Unit

data TestType = Fail | Succeed deriving (Show, Eq, Ord)
testType Fail = not
testType Succeed = identity


getParse :: Parser a -> String -> Either ParseError a
getParse p s = parse (parse_all p) "" s

unit' :: (Dimension, Value) -> Unit -> Bool
unit' x u = Quantity x == 1 `mkQu` u

qu' :: (Dimension, Value) -> Quantity -> Bool
qu' q1 q2 = Quantity q1 == q2

checkParse :: (Eq a) => Either e a -> (a -> Bool) -> Bool
checkParse (Left _) _  = False
checkParse (Right a) f = f a

testParse :: (Eq a) => TestType -> Parser a -> String -> (a -> Bool) -> TestTree
testParse t p s f = testCase (show s) $ (testType t) result @? ""
  where result = checkParse (getParse p s) f
