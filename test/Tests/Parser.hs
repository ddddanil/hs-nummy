module Tests.Parser (
  TestType(..)
, unit', qu'
, checkDim, checkUnit, checkQu
, checkParseUnit, checkParseQu
) where

import Protolude
import Data.String (String)
import Test.Tasty       (TestTree, defaultMain, testGroup, localOption, Timeout(Timeout))
import Test.Tasty.HUnit (testCase, (@?=), (@=?), (@?), Assertion, assertFailure)
import Test.Tasty.ExpectedFailure (expectFail, expectFailBecause)
import Text.Parsec hiding (parseTest)
import qualified Text.PrettyPrint.Leijen as PP

import Nummy.Parser
import Nummy.Metrology.Dimension
import Nummy.Metrology.Quantity
import Nummy.Metrology.Unit


-- TestType

data TestType = Fail | Succeed deriving (Show, Eq, Ord)

assert :: (Eq a, PP.Pretty a) => TestType -> a -> a -> Assertion
assert Succeed a b = a == b @? "assert equal\n" ++ (show . PP.pretty $ a) ++ " == " ++ (show . PP.pretty $ b)
assert Fail    a b = a /= b @? "assert not equal\n" ++ (show . PP.pretty $ a) ++ " /= " ++ (show . PP.pretty $ b)


-- Parser

getParse :: Parser a -> String -> Either ParseError a
getParse p s = runParser (parse_all p) "" "" s

checkParse :: (Eq a, PP.Pretty a) => Parser a -> TestType -> String -> a -> Assertion
checkParse p t s x =
  case getParse p s of
    Left err ->
      if t == Succeed
      then assertFailure ("failed to parse " ++ show err)
      else True @? "Expected parse failure"
    Right y -> assert t x y


-- Constructors

unit' :: (Dimension, Value) -> Unit
unit' (d, v) = conversion_ratio d v

qu' :: (Dimension, Value) -> Quantity
qu' (d, v) = mkQu v (canonical_unit d)


-- check functions

checkDim :: TestType -> String -> Dimension -> Dimension -> TestTree
checkDim t s d1 d2 = testCase s $ assert t d1 d2

checkUnit :: TestType -> String -> Unit -> Unit -> TestTree
checkUnit t s u1 u2 = testCase s $ assert t u1 u2

checkQu :: TestType -> String -> Quantity -> Quantity -> TestTree
checkQu t s q1 q2 = testCase s $ assert t q1 q2

checkParseUnit :: TestType -> String -> Unit -> TestTree
checkParseUnit t s u = testCase (show s) $ checkParse unit t s u

checkParseQu :: TestType -> String -> Quantity -> TestTree
checkParseQu t s q = testCase (show s) $ checkParse quantity t s q
