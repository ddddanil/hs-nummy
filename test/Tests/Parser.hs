module Tests.Parser (
  checkDim, checkUnit, checkQu
, checkParseUnit, checkParseQu, checkParseExpr
) where

import Protolude
import Data.String (String)
import Test.Tasty       (TestTree, defaultMain, testGroup, localOption, Timeout(Timeout))
import Test.Tasty.HUnit (testCase, (@?=), (@=?), (@?), Assertion, assertFailure)
import Test.Tasty.ExpectedFailure (expectFail, expectFailBecause)
import Text.Parsec hiding (parseTest)
import qualified Text.PrettyPrint.Leijen as PP

import Nummy.Parser
import Nummy.Metrology
import Tests.Definitions


-- Parser

getParse :: Parser a -> String -> Either ParseError a
getParse p s = runParser (parse_all p) Nothing "" s

checkParse :: (Eq a, PP.Pretty a) => Parser a -> TestType -> String -> a -> Assertion
checkParse p t s x =
  case getParse p s of
    Left err ->
      if t == Succeed
      then assertFailure ("failed to parse " ++ show err)
      else True @? "Expected parse failure"
    Right y -> assert t x y


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

checkParseExpr :: TestType -> String -> Quantity -> TestTree
checkParseExpr t s q = testCase (show s) $ checkParse expression t s q
